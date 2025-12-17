# SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
#
# well-known-ecosystem - Container Image
# Build: podman build -t well-known-ecosystem .
# Run:   podman run --rm -it well-known-ecosystem

FROM docker.io/library/alpine:3.20

LABEL org.opencontainers.image.title="well-known-ecosystem"
LABEL org.opencontainers.image.description="Well-known URI ecosystem management with RSR compliance"
LABEL org.opencontainers.image.version="0.1.0"
LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/well-known-ecosystem"
LABEL org.opencontainers.image.licenses="MIT OR AGPL-3.0-or-later"
LABEL org.opencontainers.image.authors="Jonathan D.A. Jewell <jonathan.jewell@gmail.com>"

# Install dependencies
RUN apk add --no-cache \
    guile \
    just \
    git \
    ca-certificates

# Create non-root user
RUN adduser -D -h /app appuser

WORKDIR /app

# Copy project files
COPY --chown=appuser:appuser . .

USER appuser

# Default command
CMD ["just", "--list"]
