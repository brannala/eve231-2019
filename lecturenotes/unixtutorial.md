# EVE 231 UNIX Tutorial

## Getting information and manipulating files
| UNIX Command | Description                                  | Useful Options |
|:-------------|:---------------------------------------------|:---------------|
| who          | List users that are logged in                | -u -a          |
| pwd          | Print working directory                      |                |
| ls           | List files                                   | -l -a          |
| tree         | Show files and directories as a tree         | -a -d          |
| cd           | Change directory                             |                |
| df           | Show disk space                              | -l -h          |
| du           | Show directory size                          | -s -h --time   |
| cat          | Print contents of file                       |                |
| mv           | Move file                                    |                |
| rm           | Remove file                                  | -R -f          |
| rmdir        | Remove directory (must be empty)             |                |
| cp           | Copy file                                    |                |
| more         | Display file contents sequentially           | -n +n          |
| tail         | Display contents from end of file            | -n             |
| mkdir        | Create directory                             |                |
| wc           | Count lines, words, etc                      | -l -w -m       |
| diff         | Get differences between files or directories | -q -s -y -r    |
| sort         | Sort lines of file                           | -g             |
| history      | Display history of commands                  |                |
| !n           | Execute command n from history               |                |
| echo         | Echos arguments                              |                |

## Unix processes and job control
| UNIX Command | Description                      | Useful Options |
|:-------------|:---------------------------------|:---------------|
| ps           | List processes                   | -a -e          |
| htop         | Graphical window with processes  |                |
| top          | Processes ranked by CPU usage    |                |
| nice         | Allow resource management of job |                |
| kill PID     | kill process with PID            | -9               |

## Modifying File permissions
| Unix Command | Description      | Useful Options    |
|:-------------|:-----------------|:------------------|
| chmod        | Change file mode | +x -x +r -r +w -w |
|              |                  |                   |
|              |                  |                   |
|              |                  |                   |

## UNIX Redirects and Pipes
The UNIX philosophy states that programs should should do one thing well and that there should be no difference between outputting text to the screen or to a file. Similarly programs should not care whether inputs come from a terminal or a file. A redirect send the output of a program into a file rather than to the standard output (screen). For example the redirect operator ```>``` redirects the output of a program to a file, either creating a new file or overwriting an existing file. The command
```
ls -lh > dir.txt
```
prints the directory information to a file named dir.txt. The append operator ```>>``` either writes the output to the end of an existing file or creates a new file of none exists. A pipe ```|``` is used to take the output of one program and feed it as input into another program. For example,
```
ls -lh | grep bruce | wc -l
```
counts the number of files in the working directory that are owned by user bruce. We use the program grep to filter the ```ls``` output. It prints all the lines that contain the word bruce. 
