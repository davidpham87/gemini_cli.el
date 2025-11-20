# Multiple gemini-cli in different vterm

The goal is to allow the users to launch multiple gemini-cli inside emacs.

## Why?

It is more efficient to keep the context small 

## Required features

# Configurations

The users should be able to define sequence of maps with the following keys:
  * name: the name of the vterm terminal
  * command: default to the global command of the library. This would allow to configure the models for some tasks.
  * home directory: the directory where the command should be launch
  * initial-prompt: what should be sent as the first command to contextualize the agent.

The default agent should be called gemini

## Creating an agent

From a config, modify the gemini-cli-start to accept the config map and initialize the cli accordingly
The name of the buffer should be `*gemini-{{name}}*` and should be initialized
1. change to the directory if necessary
2. Launch the command from the map
3. Execute the initial prompt

## Keeping track

Emacs should keep track of 
* the vterm that has been created through gemini-cli-start (keep a map of name and buffer).
* the latest *gemini-* vterm has been visited and should be the default buffer where the commands should be sent.

## Calling functions

However when the user hits `C-u` then emacs should asked in which buffer should the command be executed through options.

## Functionalities

- Show all the gemini vterms. Create a function that create new windows and display all the opened gemini-cli.

