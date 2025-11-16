# gemini_cli.el

A library for interacting with Gemini from Emacs.

## Motivation

Gemini as your next programming language with gemini-cli.

If LLMs are powerful tools, we want the same workflow as programmers:

1. We want to write files for instructions that can be versioned.
2. We should be able to send regions to Gemini for evaluation.
3. We want to evaluate sections interactively and effortlessly.

`gemini_cli.el is the missing piece. We are leveraging the new `gemini-cli` that provides a
powerful REPL-like environment.

This library provides some helpful functions to ease development and make
it fun to work with LLMs from Emacs.

## Shortcuts

| Shortcut  | Description                                          |
|-----------|------------------------------------------------------|
| `C-c C-p` | Start the Gemini CLI in a new terminal window.       |
| `C-c C-z` | Switch to the Gemini CLI buffer.                     |
| `C-c C-r` | Send the selected region to the Gemini CLI.          |
| `C-M-x`   | Send the current markdown section to the Gemini CLI. |
| `C-c M-p` | Move up gemini-cli window by one page                |
| `C-c M-n` | Move down gemini-cli window by one page              |
|-----------|------------------------------------------------------|

## Installation

To use `gemini-cli.el`, you first need to install the `gemini-cli` itself.

### Install Gemini CLI

YouCan install the Gemini CLI using `npm` (Node.js Package Manager) or `Homebrew`.

**Prerequisites:** Ensure you have Node.js (version 18 or higher) installed.

**Using npm (recommended for global use):**

```bash
npm install -g @google/gemini-cli
```

**Using npx (for instant, no-install run):**

```bash
npx @google/gemini-cli
```

**Using Homebrew (macOS/Linux):**

```bash
brew install gemini-cli
```

### Emacs Setup

Once `gemini-cli` is installed, you can set up `gemini_cli.el` in your Emacs configuration.

(Further instructions for Emacs setup would go here, e.g., how to load the `gemini_cli.el` file.)

## Documentation

For more detailed information on `gemini-cli`, refer to the official documentation:

*   [Gemini CLI GitHub Repository](https://github.com/google-gemini/gemini-cli)
*   [Gemini CLI Official Website](https://geminicli.cloud/)

## Reliability

Vibe coded with Gemini, tested and checked manually.
