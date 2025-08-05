# Installation Guide

This guide provides step-by-step installation instructions for the 2048 Heuristics project across different platforms.

## Prerequisites

### Required Dependencies

- **GNU Guile 3.0+** - The Scheme implementation used for this project
- **Make** - For using the build system (optional but recommended)

### Optional Dependencies

- **gnuplot** - For generating analysis visualizations
- **git** - For cloning the repository

## Platform-Specific Installation

### Ubuntu/Debian

```bash
# Update package list
sudo apt update

# Install required dependencies
sudo apt install guile-3.0 make

# Install optional dependencies
sudo apt install gnuplot git

# Clone the repository
git clone https://github.com/aygp-dr/2048-heuristics.git
cd 2048-heuristics

# Verify installation
make deps
make test
```

### FreeBSD

```bash
# Install required dependencies
sudo pkg install guile3 gmake

# Install optional dependencies  
sudo pkg install gnuplot git

# Clone the repository
git clone https://github.com/aygp-dr/2048-heuristics.git
cd 2048-heuristics

# Use gmake instead of make on FreeBSD
gmake deps
gmake test
```

### macOS

Using Homebrew:

```bash
# Install Homebrew if not already installed
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install required dependencies
brew install guile make

# Install optional dependencies
brew install gnuplot git

# Clone the repository
git clone https://github.com/aygp-dr/2048-heuristics.git
cd 2048-heuristics

# Verify installation
make deps
make test
```

### Arch Linux

```bash
# Install required dependencies
sudo pacman -S guile make

# Install optional dependencies
sudo pacman -S gnuplot git

# Clone the repository
git clone https://github.com/aygp-dr/2048-heuristics.git
cd 2048-heuristics

# Verify installation
make deps
make test
```

## Verification

After installation, verify everything is working:

```bash
# Check dependencies
make deps

# Run the complete test suite
make test

# Try running a quick analysis
make run-analysis

# Generate plots (requires gnuplot)
make plot-results
```

## Troubleshooting

### Common Issues

1. **"guile: command not found"**
   - Make sure GNU Guile 3.0+ is installed
   - On some systems, the command might be `guile3` instead of `guile`

2. **"Required file not found" error on FreeBSD**
   - Update the shebang in scripts from `#!/bin/bash` to `#!/usr/local/bin/bash`
   - Or use the provided Makefile which handles this automatically

3. **Test failures**
   - Ensure you're using GNU Guile 3.0 or later
   - Check that all SRFI modules are available: `guile -c "(use-modules (srfi srfi-64))"`

4. **"gnuplot not found" during plot generation**
   - Install gnuplot: `sudo apt install gnuplot` (Ubuntu/Debian)
   - Plot generation is optional - analysis still works without it

### Getting Help

If you encounter issues not covered here:

1. Check the existing [GitHub Issues](https://github.com/aygp-dr/2048-heuristics/issues)
2. Run `make info` to see system information
3. Create a new issue with your system details and error messages

## Development Setup

For development work:

```bash
# Install additional development tools
make deps

# Run tests in watch mode (requires inotify-tools)
make watch

# Run with profiling
make profile

# Build optimized version
make build
make install
```

## Next Steps

After successful installation:

1. Read the [README.md](README.md) for usage examples
2. Check [ANALYSIS.md](ANALYSIS.md) for detailed strategy analysis
3. Explore the `examples/` directory for sample usage
4. Review the [CONTRIBUTING.org](CONTRIBUTING.org) guide if you want to contribute