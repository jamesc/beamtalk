# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Components.CoreComponentsTest do
  @moduledoc """
  Direct component tests for `BtAttachWeb.CoreComponents` (BT-3294).

  These shared function components (buttons, modals, badges, form inputs,
  etc.) were previously only rendered incidentally as a side effect of
  whichever LiveView page happened to use them, so most prop/slot/conditional
  branches were never exercised. This suite renders each component directly
  via `Phoenix.LiveViewTest.rendered_to_string/1` (for components with slots)
  or `render_component/2` (for the plain functions), covering the meaningful
  variants of each — no workspace node or browser needed.
  """
  use ExUnit.Case, async: true

  import Phoenix.Component
  import Phoenix.LiveViewTest

  alias BtAttachWeb.CoreComponents, as: CC
  alias Phoenix.LiveView.JS

  # ── modal/1 ────────────────────────────────────────────────────────────

  describe "modal/1" do
    test "hidden by default, with no phx-mounted JS pipeline" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.modal id="confirm-modal">Are you sure?</CC.modal>
        """)

      doc = Floki.parse_fragment!(html)

      assert Floki.attribute(doc, "#confirm-modal", "class") == ["relative z-50 hidden"]
      assert html =~ "Are you sure?"
      # @show is false, so `@show && show_modal(@id)` short-circuits: the
      # attribute is absent entirely (not merely empty).
      assert Floki.attribute(doc, "#confirm-modal", "phx-mounted") == []
    end

    test "show: true threads the show_modal/1 JS pipeline onto phx-mounted" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.modal id="confirm-modal" show>Are you sure?</CC.modal>
        """)

      doc = Floki.parse_fragment!(html)
      [mounted] = Floki.attribute(doc, "#confirm-modal", "phx-mounted")

      # focus_first + add_class("overflow-hidden") are only emitted by
      # show_modal/1's chain — unlike hide_modal/1's chain on phx-remove
      # (always present, see the sibling test above), so this pins the
      # branch to the JS op that's unique to it.
      assert mounted =~ "focus_first"
      assert mounted =~ "overflow-hidden"
    end

    test "custom on_cancel is threaded into the close affordances" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.modal id="confirm-modal" on_cancel={JS.navigate("/posts")}>Bye</CC.modal>
        """)

      assert html =~ "/posts"
      assert html =~ "data-cancel"
    end
  end

  # ── flash/1 ────────────────────────────────────────────────────────────

  describe "flash/1" do
    test "renders slot content with info styling and the default id" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.flash kind={:info} flash={%{}}>Welcome Back!</CC.flash>
        """)

      assert html =~ ~s(id="flash-info")
      assert html =~ "Welcome Back!"
      assert html =~ "bg-emerald-50"
    end

    test "renders a flash-map message with error styling and a title icon" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.flash kind={:error} flash={%{"error" => "Boom"}} title="Error!" />
        """)

      assert html =~ "Boom"
      assert html =~ "Error!"
      assert html =~ "shadow-md"
      assert html =~ "hero-exclamation-circle-mini"
    end

    test "renders nothing when there is no slot content and no flash message" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.flash kind={:info} flash={%{}} />
        """)

      assert String.trim(html) == ""
    end

    test "forwards a custom id and arbitrary rest attributes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.flash id="custom-flash" kind={:info} flash={%{}} data-test="marker">Hi</CC.flash>
        """)

      assert html =~ ~s(id="custom-flash")
      assert html =~ ~s(data-test="marker")
    end
  end

  # ── flash_group/1 ──────────────────────────────────────────────────────

  describe "flash_group/1" do
    test "composes info/error banners from the flash map plus the always-on reconnect banners" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.flash_group flash={%{"info" => "Saved!", "error" => "Oops!"}} />
        """)

      assert html =~ "Saved!"
      assert html =~ "Oops!"
      assert html =~ "client-error"
      assert html =~ "server-error"
      assert html =~ "Attempting to reconnect"
      assert html =~ "Hang in there"
    end

    test "accepts a custom container id" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.flash_group flash={%{}} id="custom-flash-group" />
        """)

      assert html =~ ~s(id="custom-flash-group")
    end
  end

  # ── simple_form/1 ──────────────────────────────────────────────────────

  describe "simple_form/1" do
    test "binds the form to the inner_block and renders unnamespaced field names by default" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.simple_form :let={f} for={%{"name" => "Ada"}}>
          <CC.input field={f[:name]} label="Name" />
        </CC.simple_form>
        """)

      assert html =~ "Name"
      assert html =~ ~s(name="name")
    end

    test "as: namespaces field names, and the actions slot renders" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.simple_form :let={f} for={%{"name" => "Ada"}} as="user">
          <CC.input field={f[:name]} label="Name" />
          <:actions>
            <CC.button>Save</CC.button>
          </:actions>
        </CC.simple_form>
        """)

      assert html =~ ~s(name="user[name]")
      assert html =~ "Save"
    end

    test "forwards phx-* rest attributes to the underlying <form>" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.simple_form :let={_f} for={%{}} phx-change="validate" phx-submit="save">
          <p>body</p>
        </CC.simple_form>
        """)

      assert html =~ ~s(phx-change="validate")
      assert html =~ ~s(phx-submit="save")
    end
  end

  # ── button/1 ───────────────────────────────────────────────────────────

  describe "button/1" do
    test "renders inner content with no type by default" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.button>Send!</CC.button>
        """)

      assert html =~ "Send!"
      refute html =~ "type="
    end

    test "accepts a type, extra class, and rest attributes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.button type="submit" class="ml-2" disabled>Send!</CC.button>
        """)

      assert html =~ ~s(type="submit")
      assert html =~ "ml-2"
      assert html =~ "disabled"
    end
  end

  # ── input/1 ────────────────────────────────────────────────────────────

  describe "input/1 with a Phoenix.HTML.FormField" do
    # input/1's `field:` clause only shows `field.errors` when
    # `Phoenix.Component.used_input?/1` is true — which keys off whether the
    # field's name is present in the form's `params` (i.e. the client has
    # touched/submitted it), not merely whether the underlying data has
    # errors. The two tests below hold `errors:` constant and vary only
    # whether `"email"` is a param key, to make that mechanism the thing
    # under test rather than an incidental side effect of `to_form/2`.
    test "a used field shows its errors and the rose error styling" do
      form =
        to_form(%{"email" => "taken@example.com"},
          as: "user",
          errors: [email: {"has already been taken", []}]
        )

      assigns = %{form: form}

      html =
        rendered_to_string(~H"""
        <CC.input field={@form[:email]} label="Email" />
        """)

      doc = Floki.parse_fragment!(html)

      assert html =~ "has already been taken"
      assert html =~ ~s(name="user[email]")
      assert html =~ ~s(value="taken@example.com")
      assert html =~ "border-rose-400"
      assert Floki.find(doc, "p.text-rose-600") != []
    end

    test "an unused field suppresses its errors and keeps the zinc styling" do
      form = to_form(%{}, as: "user", errors: [email: {"can't be blank", []}])
      assigns = %{form: form}

      html =
        rendered_to_string(~H"""
        <CC.input field={@form[:email]} label="Email" />
        """)

      doc = Floki.parse_fragment!(html)

      # Structural, not string-matching the message: an escaping change to
      # "can't" (`&#39;` vs `&#x27;`) must not make this pass for the wrong
      # reason.
      assert Floki.find(doc, "p.text-rose-600") == []
      assert html =~ "border-zinc-300"
    end

    test "an explicit id overrides the field's own id" do
      form = to_form(%{"email" => "a@b.com"}, as: "user")
      assigns = %{form: form}

      html =
        rendered_to_string(~H"""
        <CC.input id="custom-email-id" field={@form[:email]} label="Email" />
        """)

      assert html =~ ~s(id="custom-email-id")
      refute html =~ ~s(id="user_email")
    end

    test "multiple: true on a field-backed select suffixes the name with []" do
      form = to_form(%{"tags" => ["x"]}, as: "post")
      assigns = %{form: form}

      html =
        rendered_to_string(~H"""
        <CC.input type="select" field={@form[:tags]} label="Tags" multiple options={["x", "y"]} />
        """)

      assert html =~ ~s(name="post[tags][]")
    end
  end

  describe "input/1 type: checkbox" do
    test "unchecked by default" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.input type="checkbox" name="accept" label="Accept terms" value={false} />
        """)

      refute html =~ "checked"
      assert html =~ "Accept terms"
    end

    test "checked when the value is truthy, and rest/disabled is forwarded" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.input type="checkbox" name="accept" label="Accept terms" value={true} disabled />
        """)

      assert html =~ "checked"
      assert html =~ "disabled"
    end

    test "an explicit checked: false wins over a truthy value" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.input type="checkbox" name="accept" label="Accept terms" value={true} checked={false} />
        """)

      refute html =~ "checked"
    end

    test "renders its errors like the other input clauses" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.input type="checkbox" name="accept" label="Accept terms" errors={["must be accepted"]} />
        """)

      assert html =~ "must be accepted"
    end
  end

  describe "input/1 type: select" do
    test "without a prompt or multiple" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.input type="select" name="color" label="Color" value="Blue" options={["Red", "Blue"]} />
        """)

      assert html =~ "Color"
      assert html =~ "Red"
      assert html =~ "Blue"
      refute html =~ "multiple"
    end

    test "with a prompt and multiple" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.input
          type="select"
          name="color"
          label="Color"
          value={["Blue"]}
          prompt="Choose one"
          multiple
          options={["Red", "Blue"]}
        />
        """)

      assert html =~ "Choose one"
      assert html =~ "multiple"
    end

    test "renders its errors like the other input clauses" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.input
          type="select"
          name="color"
          label="Color"
          value={nil}
          options={["Red"]}
          errors={["is required"]}
        />
        """)

      assert html =~ "is required"
    end
  end

  describe "input/1 type: textarea" do
    test "renders the value and zinc border when there are no errors" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.input type="textarea" name="bio" label="Bio" value="hello" />
        """)

      assert html =~ "hello"
      assert html =~ "border-zinc-300"
    end

    test "renders the rose border when there are errors" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.input type="textarea" name="bio" label="Bio" value="" errors={["too short"]} />
        """)

      assert html =~ "too short"
      assert html =~ "border-rose-400"
    end
  end

  describe "input/1 default clause (text/email/etc.)" do
    test "renders label, value and no errors by default" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.input type="email" name="email" label="Email" value="a@b.com" />
        """)

      assert html =~ ~s(type="email")
      assert html =~ "a@b.com"
      assert html =~ "Email"
      assert html =~ "border-zinc-300"
    end

    test "accepts a bare name + errors without a field, and rest attributes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.input name="my-input" value="" errors={["oh no!"]} placeholder="type here" required />
        """)

      assert html =~ "oh no!"
      assert html =~ "border-rose-400"
      assert html =~ ~s(placeholder="type here")
      assert html =~ "required"
    end
  end

  # ── label/1 ────────────────────────────────────────────────────────────

  describe "label/1" do
    test "renders the for attribute and inner content when given" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.label for="email">Email</CC.label>
        """)

      assert html =~ ~s(for="email")
      assert html =~ "Email"
    end

    test "omits the for attribute by default" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.label>Plain label</CC.label>
        """)

      refute html =~ ~s(for=)
      assert html =~ "Plain label"
    end
  end

  # ── error/1 ────────────────────────────────────────────────────────────

  describe "error/1" do
    test "renders the icon and the message" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.error>Required</CC.error>
        """)

      assert html =~ "Required"
      assert html =~ "hero-exclamation-circle-mini"
    end
  end

  # ── header/1 ───────────────────────────────────────────────────────────

  describe "header/1" do
    test "plain title with no actions or subtitle" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.header>Settings</CC.header>
        """)

      assert html =~ "Settings"
      refute html =~ "justify-between"
    end

    test "renders the subtitle slot when given" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.header>
          Settings
          <:subtitle>Manage your account</:subtitle>
        </CC.header>
        """)

      assert html =~ "Manage your account"
    end

    test "adds the justify-between layout when the actions slot is given, and a custom class" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.header class="custom-hdr">
          Settings
          <:actions>
            <button>Go</button>
          </:actions>
        </CC.header>
        """)

      assert html =~ "justify-between"
      assert html =~ "custom-hdr"
      assert html =~ "Go"
    end
  end

  # ── table/1 ────────────────────────────────────────────────────────────

  describe "table/1" do
    # table/1 also special-cases `rows` being a `%Phoenix.LiveView.LiveStream{}`
    # (default row_id + `phx-update="stream"`). That struct is deliberately
    # not exercised here: its constructor lives in a `@moduledoc false`
    # module with no stable public API outside of `Phoenix.Component.stream/4`
    # on a real mounted socket, so reaching into it directly would trade one
    # kind of test fragility (assertions on markup) for a worse one
    # (assertions on an undocumented internal struct shape). The plain-list
    # path below is what every non-stream caller of `<.table>` exercises.
    test "renders columns and rows with no row_click and no action slot" do
      assigns = %{rows: [%{id: 1, name: "Ada"}]}

      html =
        rendered_to_string(~H"""
        <CC.table id="people" rows={@rows}>
          <:col :let={r} label="Name">{r.name}</:col>
        </CC.table>
        """)

      assert html =~ "Name"
      assert html =~ "Ada"
      refute html =~ "phx-click"
      refute html =~ "sr-only"
    end

    test "row_click adds phx-click + hover styling, and the action slot renders its header + cell" do
      assigns = %{
        rows: [%{id: 1, name: "Ada"}],
        row_click: fn row -> JS.push("select", value: %{id: row.id}) end
      }

      html =
        rendered_to_string(~H"""
        <CC.table id="people" rows={@rows} row_click={@row_click}>
          <:col :let={r} label="Name">{r.name}</:col>
          <:action :let={r}>
            <button>Edit {r.name}</button>
          </:action>
        </CC.table>
        """)

      assert html =~ "phx-click"
      assert html =~ "select"
      assert html =~ "hover:cursor-pointer"
      assert html =~ "sr-only"
      assert html =~ "Edit Ada"
    end

    test "row_id and row_item customize the DOM id and the value passed to slots" do
      assigns = %{rows: [%{id: 1, name: "ada"}]}

      html =
        rendered_to_string(~H"""
        <CC.table
          id="people"
          rows={@rows}
          row_id={fn r -> "person-#{r.id}" end}
          row_item={fn r -> Map.update!(r, :name, &String.upcase/1) end}
        >
          <:col :let={r} label="Name">{r.name}</:col>
        </CC.table>
        """)

      assert html =~ ~s(id="person-1")
      assert html =~ "ADA"
    end
  end

  # ── list/1 ─────────────────────────────────────────────────────────────

  describe "list/1" do
    test "renders each item's title and content" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.list>
          <:item title="Views">42</:item>
          <:item title="Likes">7</:item>
        </CC.list>
        """)

      assert html =~ "Views"
      assert html =~ "42"
      assert html =~ "Likes"
      assert html =~ "7"
    end
  end

  # ── back/1 ─────────────────────────────────────────────────────────────

  describe "back/1" do
    test "renders the navigate link, inner content, and icon" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.back navigate="/posts">Back to posts</CC.back>
        """)

      assert html =~ "Back to posts"
      assert html =~ "/posts"
      assert html =~ "hero-arrow-left-solid"
    end
  end

  # ── icon/1 ─────────────────────────────────────────────────────────────

  describe "icon/1" do
    test "combines the hero- name with a custom class" do
      html = render_component(&CC.icon/1, name: "hero-x-mark-solid", class: "h-5 w-5")

      assert html =~ "hero-x-mark-solid"
      assert html =~ "h-5 w-5"
    end

    test "renders with no class by default" do
      html = render_component(&CC.icon/1, name: "hero-arrow-path")

      assert html =~ "hero-arrow-path"
    end

    test "a non hero- name has no matching clause" do
      # icon/1 only defines a `"hero-" <> _` head — documents the contract
      # that this component is heroicons-only, not a generic icon renderer.
      assert_raise FunctionClauseError, fn ->
        render_component(&CC.icon/1, name: "mdi-close")
      end
    end
  end

  # ── JS command helpers ─────────────────────────────────────────────────

  describe "show/1 and hide/1 JS builders" do
    test "show/1 emits a 300ms show op targeting the selector, with a transition triple" do
      encoded = CC.show("#flash") |> Phoenix.HTML.Safe.to_iodata() |> IO.iodata_to_binary()

      assert encoded =~ ~s(&quot;show&quot;)
      assert encoded =~ ~s(&quot;to&quot;:&quot;#flash&quot;)
      assert encoded =~ ~s(&quot;time&quot;:300)
      assert encoded =~ "duration-300"
      assert encoded =~ "opacity-100"
    end

    test "hide/1 emits a 200ms hide op targeting the selector, with a transition triple" do
      encoded = CC.hide("#flash") |> Phoenix.HTML.Safe.to_iodata() |> IO.iodata_to_binary()

      assert encoded =~ ~s(&quot;hide&quot;)
      assert encoded =~ ~s(&quot;to&quot;:&quot;#flash&quot;)
      assert encoded =~ ~s(&quot;time&quot;:200)
      assert encoded =~ "duration-200"
      assert encoded =~ "opacity-0"
    end

    test "show/2 and hide/2 thread an existing JS struct instead of starting a fresh one" do
      encoded =
        %JS{}
        |> JS.push("noop")
        |> CC.show("#a")
        |> CC.hide("#b")
        |> Phoenix.HTML.Safe.to_iodata()
        |> IO.iodata_to_binary()

      assert encoded =~ ~s(&quot;push&quot;)
      assert encoded =~ ~s(&quot;show&quot;)
      assert encoded =~ ~s(&quot;hide&quot;)
      assert encoded =~ "#a"
      assert encoded =~ "#b"
    end

    test "show_modal/1 and hide_modal/1 compose show/hide + class + focus ops for the given id" do
      shown = CC.show_modal("confirm") |> Phoenix.HTML.Safe.to_iodata() |> IO.iodata_to_binary()
      hidden = CC.hide_modal("confirm") |> Phoenix.HTML.Safe.to_iodata() |> IO.iodata_to_binary()

      assert shown =~ "#confirm-bg"
      assert shown =~ "#confirm-container"
      assert shown =~ ~s(&quot;add_class&quot;)
      assert shown =~ "overflow-hidden"
      assert shown =~ ~s(&quot;focus_first&quot;)

      assert hidden =~ "#confirm-bg"
      assert hidden =~ "#confirm-container"
      assert hidden =~ ~s(&quot;remove_class&quot;)
      assert hidden =~ ~s(&quot;pop_focus&quot;)
    end
  end

  # ── translate_error/1, translate_errors/2 ────────────────────────────

  describe "translate_error/1" do
    test "interpolates count/opts placeholders into the message" do
      assert CC.translate_error({"must be %{count} characters", [count: 5]}) ==
               "must be 5 characters"
    end

    test "returns the message unchanged when there are no opts" do
      assert CC.translate_error({"is invalid", []}) == "is invalid"
    end
  end

  describe "translate_errors/2" do
    test "returns only the translated messages for the given field" do
      errors = [email: {"can't be blank", []}, name: {"is invalid", []}]

      assert CC.translate_errors(errors, :email) == ["can't be blank"]
    end

    test "returns an empty list when the field has no errors" do
      errors = [name: {"is invalid", []}]

      assert CC.translate_errors(errors, :email) == []
    end
  end
end
