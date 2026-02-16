# Security Policy

## Reporting Security Issues

**Please do not report security vulnerabilities through public GitHub issues.**

Instead, please report them via GitHub's private security advisory feature:
https://github.com/jamesc/beamtalk/security/advisories/new

When reporting a security vulnerability, please include:

- **Description** - Clear explanation of the vulnerability
- **Steps to reproduce** - Detailed instructions to reproduce the issue
- **Potential impact** - What could an attacker do with this vulnerability?
- **Affected versions** - Which versions are vulnerable (if known)
- **Suggested fix** - Proposed solution (if you have one)

We will respond to security reports within **48 hours** and aim to release fixes within 7 days for critical issues.

## Supported Versions

Beamtalk is currently in **pre-1.0 development**. Security updates will be applied to the latest commit on the `main` branch.

| Version | Supported          |
| ------- | ------------------ |
| main    | :white_check_mark: |
| < 1.0   | :x:                |

Once Beamtalk reaches 1.0, we will maintain security updates for:
- Current major version (latest)
- Previous major version (critical fixes only)

## Security Best Practices

When using Beamtalk, follow these security guidelines:

### Dependency Management
- Keep dependencies up to date using `cargo update`
- Run `cargo audit` regularly to check for known vulnerabilities
- Review dependency changes in pull requests

### Development Environment
- Never commit `.env` files or API tokens to git
- Use environment variables for sensitive configuration
- Keep your `GH_TOKEN` and `LINEAR_API_TOKEN` secure

### Code Contributions
- Validate all user input at system boundaries
- Return errors instead of panicking on invalid input
- Document any `unsafe` code with `// SAFETY:` comments
- Follow the architecture principles in `docs/development/architecture-principles.md`

## Security Scanning

This repository uses:
- **GitHub Dependabot** - Automated dependency updates and security alerts
- **GitHub Advanced Security** - Secret scanning

## Acknowledgments

We appreciate responsible disclosure from security researchers. Contributors who report valid security issues will be acknowledged in release notes (unless they prefer to remain anonymous).
