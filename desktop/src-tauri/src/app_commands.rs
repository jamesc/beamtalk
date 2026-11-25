// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

// The app's own IPC commands, named once (BT-5194). Plain `//` comments,
// not `//!`: `build.rs` `include!`s this file, where an inner doc comment
// is not allowed.
//
// `build.rs` declares every name below in the
// app's `AppManifest`, which generates one `allow-<command>` permission per
// command. Declaring a manifest is what makes Tauri check *app* commands
// against the capabilities at all: with no manifest, Tauri 2 dispatches an
// app command from any local-origin webview without an ACL lookup (see
// `tauri::webview::Webview::on_message`). `capabilities/default.json`
// grants the `allow-*` permissions to the `picker` window only, so a
// workspace window cannot invoke them, whatever page it ends up showing.
//
// `main.rs`'s `generate_handler!` list must name the same commands; the
// `handler_registers_exactly_the_declared_commands` test below enforces it.

/// Every command `main.rs` registers with `generate_handler!`.
pub const APP_COMMANDS: &[&str] = &[
    "list_workspaces",
    "attach",
    "detach",
    "create_workspace",
    "quit",
    "get_launcher_logs",
];

#[cfg(test)]
mod tests {
    use super::APP_COMMANDS;

    use tauri::test::{INVOKE_KEY, MockRuntime, mock_builder};
    use tauri::webview::InvokeRequest;
    use tauri::{App, Manager, WebviewUrl, WebviewWindow, WebviewWindowBuilder};

    /// The URL a page served from the app's own bundled frontend reports
    /// (Tauri's `tauri://localhost`, `http://tauri.localhost` on Windows).
    fn local_url() -> tauri::Url {
        if cfg!(windows) {
            "http://tauri.localhost/index.html"
        } else {
            "tauri://localhost/index.html"
        }
        .parse()
        .unwrap()
    }

    /// The URL a workspace window loads (`commands::attach_and_open_window`).
    fn workspace_url() -> tauri::Url {
        "http://localhost:4567/".parse().unwrap()
    }

    /// The real capabilities and app manifest (`generate_context!` reads
    /// `tauri.conf.json`, `capabilities/` and the manifest `build.rs`
    /// declared), with a stub handler that accepts every command. The ACL
    /// check runs before any handler, so a refusal here is Tauri's ACL and
    /// nothing else; a command that passes the ACL just answers `"ran"`,
    /// without running the real command's side effects.
    fn app() -> App<MockRuntime> {
        mock_builder()
            .invoke_handler(|invoke| {
                invoke.resolver.resolve("ran");
                true
            })
            .build(tauri::generate_context!(test = true))
            .expect("build mock app")
    }

    fn window(app: &App<MockRuntime>, label: &str, url: WebviewUrl) -> WebviewWindow<MockRuntime> {
        WebviewWindowBuilder::new(app, label, url)
            .build()
            .expect("build mock window")
    }

    fn invoke(
        webview: &WebviewWindow<MockRuntime>,
        cmd: &str,
        url: tauri::Url,
    ) -> Result<serde_json::Value, serde_json::Value> {
        tauri::test::get_ipc_response(
            webview,
            InvokeRequest {
                cmd: cmd.into(),
                callback: tauri::ipc::CallbackFn(0),
                error: tauri::ipc::CallbackFn(1),
                url,
                body: tauri::ipc::InvokeBody::default(),
                headers: tauri::http::HeaderMap::default(),
                invoke_key: INVOKE_KEY.to_string(),
            },
        )
        .map(|body| body.deserialize::<serde_json::Value>().unwrap())
    }

    fn workspace_label() -> String {
        beamtalk_desktop_shell::attach::window_label("abc123")
    }

    #[test]
    fn the_picker_may_invoke_every_app_command() {
        let app = app();
        let picker = app
            .get_webview_window("picker")
            .unwrap_or_else(|| window(&app, "picker", WebviewUrl::App("index.html".into())));
        for cmd in APP_COMMANDS {
            assert_eq!(
                invoke(&picker, cmd, local_url()),
                Ok(serde_json::json!("ran")),
                "the picker must be allowed to invoke `{cmd}`"
            );
        }
    }

    /// The workspace window shows workspace and user content from the
    /// front's localhost server. Script there must not reach the app's
    /// commands (quit, attach, detach, create a workspace, read the
    /// launcher log).
    #[test]
    fn a_workspace_window_is_refused_every_app_command() {
        let app = app();
        let workspace = window(&app, &workspace_label(), WebviewUrl::External(workspace_url()));
        for cmd in APP_COMMANDS {
            assert!(
                invoke(&workspace, cmd, workspace_url()).is_err(),
                "a workspace window's page must not invoke `{cmd}`"
            );
        }
    }

    /// The same window, navigated to a page of the app's own frontend (a
    /// local origin). Before BT-5194 this was allowed: with no app manifest,
    /// Tauri never consulted the capabilities for an app command from a
    /// local origin, so the `"windows": ["picker"]` scoping did nothing.
    #[test]
    fn a_workspace_window_on_a_local_page_is_refused_every_app_command() {
        let app = app();
        let workspace = window(&app, &workspace_label(), WebviewUrl::External(workspace_url()));
        for cmd in APP_COMMANDS {
            assert!(
                invoke(&workspace, cmd, local_url()).is_err(),
                "a non-picker window must not invoke `{cmd}`, even from a local page"
            );
        }
    }

    /// The picker window itself, if something navigated it off the bundled
    /// frontend (to a remote page), must not keep its command access.
    #[test]
    fn the_picker_on_a_remote_page_is_refused_every_app_command() {
        let app = app();
        let picker = app
            .get_webview_window("picker")
            .unwrap_or_else(|| window(&app, "picker", WebviewUrl::App("index.html".into())));
        for cmd in APP_COMMANDS {
            assert!(
                invoke(&picker, cmd, workspace_url()).is_err(),
                "a remote page in the picker must not invoke `{cmd}`"
            );
        }
    }

    /// `withGlobalTauri` is app-wide: it would inject the `window.__TAURI__`
    /// API into every workspace window too. The picker uses
    /// `ui/tauri-bridge.js` instead.
    #[test]
    fn the_global_tauri_api_is_not_injected() {
        assert!(!app().config().app.with_global_tauri);
    }

    /// `main.rs`'s `generate_handler!` list and [`APP_COMMANDS`] (the
    /// manifest `build.rs` declares) must name the same commands: a command
    /// missing from the manifest is refused to the picker too, and a
    /// manifest entry with no handler is dead.
    #[test]
    fn handler_registers_exactly_the_declared_commands() {
        let main_rs = include_str!("main.rs");
        let start = main_rs
            .find("generate_handler![")
            .expect("main.rs registers its commands with generate_handler!");
        let rest = &main_rs[start + "generate_handler![".len()..];
        let list = &rest[..rest.find(']').expect("generate_handler! list closes")];
        let mut registered: Vec<&str> = list
            .split(',')
            .map(str::trim)
            .filter(|s| !s.is_empty())
            .map(|s| s.rsplit("::").next().unwrap())
            .collect();
        let mut declared = APP_COMMANDS.to_vec();
        registered.sort_unstable();
        declared.sort_unstable();
        assert_eq!(registered, declared);
    }

    /// Every declared command is granted to the picker by name in the
    /// capability file (`allow-<command>`, kebab-case, as `tauri-build`
    /// generates it), and nothing grants them to any other window.
    #[test]
    fn the_capability_grants_the_commands_to_the_picker_only() {
        let capability: serde_json::Value =
            serde_json::from_str(include_str!("../capabilities/default.json")).unwrap();
        assert_eq!(capability["windows"], serde_json::json!(["picker"]));
        assert!(capability.get("webviews").is_none());
        assert!(capability.get("remote").is_none());
        let permissions: Vec<&str> = capability["permissions"]
            .as_array()
            .unwrap()
            .iter()
            .filter_map(serde_json::Value::as_str)
            .collect();
        for cmd in APP_COMMANDS {
            let permission = format!("allow-{}", cmd.replace('_', "-"));
            assert!(
                permissions.contains(&permission.as_str()),
                "capabilities/default.json must grant `{permission}`"
            );
        }
    }
}
