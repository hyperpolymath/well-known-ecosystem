# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 0.1.x   | :white_check_mark: |

## Reporting a Vulnerability

We take security vulnerabilities seriously. If you discover a security issue, please report it responsibly.

### How to Report

1. **Email**: Send details to [security@hyperpolymath.org](mailto:security@hyperpolymath.org)
2. **Encryption**: Use our [GPG key](https://hyperpolymath.org/gpg/security.asc) for sensitive reports
3. **Do NOT** open public GitHub issues for security vulnerabilities

### What to Include

- Description of the vulnerability
- Steps to reproduce
- Potential impact
- Suggested fix (if any)

### Response Timeline

- **Acknowledgment**: Within 48 hours
- **Initial Assessment**: Within 7 days
- **Resolution Target**: Within 30 days (depending on severity)

### Disclosure Policy

- We follow coordinated disclosure practices
- Security researchers will be credited (unless anonymity is preferred)
- Public disclosure occurs after a fix is released

## Security Measures

This project follows RSR (Rhodium Standard Repository) security guidelines:

- **SAST**: CodeQL scanning on all commits
- **Dependencies**: Automated security updates via Dependabot
- **Secrets**: Environment variables only (no hardcoded credentials)
- **Hashing**: SHA-256 or stronger (no MD5/SHA1 for security purposes)
- **Transport**: HTTPS only (no HTTP URLs)
- **Actions**: SHA-pinned GitHub Actions

## Preferred Languages

- English
- Dutch (Nederlands)

## Acknowledgments

Security researchers who responsibly disclose vulnerabilities are acknowledged at:
https://hyperpolymath.org/security/acknowledgments
