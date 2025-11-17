# gemini-cli-mode: Emacs interface to gemini cli

A library for interacting with Gemini from Emacs. Provide a `gemini` mode with
shortcuts to

1. To create a vterm with gemini-cli, and switching to it easily.
2. Send region / mardown section to gemini-cli
3. Move up/down vterm window

This is just to replicate the repl driven workflow on Clojure, but with Gemini
as backend.

## Motivation

Gemini as your next programming language with gemini-cli.

If LLMs are powerful tools, we want the same workflow as programmers:

1. We want to write files for instructions that can be versioned.
2. We should be able to send regions to Gemini for evaluation.
3. We want to evaluate sections interactively and effortlessly.

`gemini_cli_mode` is the missing piece. We are leveraging the new `gemini-cli` that provides a
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
| `C-c M-p` | Page up gemini-cli window by one page                |
| `C-c M-n` | Page down gemini-cli window by one page              |


## Usage

Once `gemini-cli-mode` is installed and enabled, you can interact with the Gemini CLI from any buffer. Here are some common workflows:

1.  **Start the Gemini CLI:**
    Press `C-c C-p` to open a new `vterm` buffer with the `gemini` command running.

2.  **Switch to the Gemini CLI:**
    Press `C-c C-z` to quickly jump to the `*gemini-cli*` buffer.

3.  **Send a region of text:**
    Select a region of text in any buffer and press `C-c C-r` to send it to the Gemini CLI. This is useful for sending code snippets or questions.

4.  **Send a markdown section:**
    When editing a markdown file, you can send the current section (the text under the current heading) to the Gemini CLI by pressing `C-M-x`.

5.  **Navigate the Gemini CLI:**
    You can use `C-c M-p` and `C-c M-n` to page up and down in the `*gemini-cli*` buffer, which is useful when the output is long.

## Installation

To use `gemini-cli.el`, you first need to install the `gemini-cli` itself.

### Install Gemini CLI

You can install the Gemini CLI using `npm` (Node.js Package Manager) or `Homebrew`.

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

Once `gemini-cli` is installed, you can set up `gemini-cli-mode` in your Emacs
configuration.

You will need to install the following dependencies.

* [vterm](https://github.com/akermu/emacs-libvterm)
* Markdown mode

For now you can just download the file and execute it on your path.

## Configuration

You can customize the behavior of `gemini-cli-mode` by setting the following variables in your Emacs configuration:

*   `gemini-cli-buffer`: The name of the buffer for the Gemini CLI process. The default is `"*gemini-cli*"`.

For example, to change the default keybindings, you can add the following to your `init.el` file:

```emacs-lisp
(with-eval-after-load 'gemini-cli-mode
  (define-key gemini-cli-mode-map (kbd "C-c g p") 'gemini-cli-start)
  (define-key gemini-cli-mode-map (kbd "C-c g z") 'gemini-cli-switch-buffer))
```

## Documentation

For more detailed information on `gemini-cli`, refer to the official documentation:

*   [Gemini CLI GitHub Repository](https://github.com/google-gemini/gemini-cli)
*   [Gemini CLI Official Website](https://geminicli.cloud/)

## Reliability

Vibe coded with Gemini, tested and checked manually.
