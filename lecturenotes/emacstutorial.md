# EVE 231 Emacs Tutorial

## Getting Started

To start using Emacs open a unix terminal and type ```emacs```. You will see a splash screen similar to the following that tells you the version of Emacs that you are using and some other useful information
```
Welcome to GNU Emacs, one component of the GNU/Linux operating system.                                                                                                              
                                                                                                                                                                                    
Get help           C-h  (Hold down CTRL and press h)                                                                                                                                
Emacs manual       C-h r        Browse manuals     C-h i                                                                                                                            
Emacs tutorial     C-h t        Undo changes       C-x u                                                                                                                            
Buy manuals        C-h RET      Exit Emacs         C-x C-c                                                                                                                          
Activate menubar   M-`                                                                                                                                                              
(‘C-’ means use the CTRL key.  ‘M-’ means use the Meta (or Alt) key.                                                                                                                
If you have no Meta key, you may instead type ESC followed by the character.)                                                                                                       
Useful tasks:                                                                                                                                                                       
Visit New File                  Open Home Directory                                                                                                                                 
Customize Startup               Open *scratch* buffer                                                                                                                               
                                                                                                                                                                                    
GNU Emacs 26.3 (build 1, x86_64-pc-linux-gnu)                                                                                                                                       
 of 2019-09-16                                                                                                                                                                      
Copyright (C) 2019 Free Software Foundation, Inc.                                                                                                                                   
                                                                                                                                                                                    
GNU Emacs comes with ABSOLUTELY NO WARRANTY; type C-h C-w for full details.                                                                                                         
Emacs is Free Software--Free as in Freedom--so you can redistribute copies                                                                                                          
of Emacs and modify it; type C-h C-c to see the conditions.                                                                                                                         
Type C-h C-o for information on getting the latest version.                                                                                                                         
                                                                                                                                                                                    
If an Emacs session crashed recently, type M-x recover-session RET                                                                                                                  
to recover the files you were editing.  
```
The first thing you need to understand is that Emacs uses combinations of special keyboard keys to initiate Emacs commands. The two most important keys are the 
```Control``` key symbolized as ```C``` and the ```Meta``` key symbolized as ```M```. Thus ```C-h``` means hold down the ```Control``` key and press ```h```. This initiates the Emacs help system. To read the Emacs manual the command is ```C-h r``` which means hold down the Control key and press ```h```, then release and press ```r```. The Meta key varies among keyboards/computers. On Linux machines the Meta key is usually the ```Alt``` key. On the Mac OS X it is usually either the ```Option``` or ```Command``` key. For example, if the cursor is at the beginning of a word you can delete the word with the command ```M-d``` which could be executed on a Linux machine by holding down the ```Alt``` key and pressing ```d```. On any computer you can also use the ```ESC``` key in place of the Meta key. However,  if you do that don't continue to hold down the ESC just press it once.  Three commands are particularly useful: ```C-x u``` which will undo the previous command and can be applied recursively to go back several commands, ```C-g``` which aborts whatever command you are running, and ```C-x C-c``` which exits Emacs.

### Exercise 1

Work through the Emacs tutorial which should take about 30 minutes. To start the Emacs tutorial type the command ```C-h t```.

### Emacs Resources

It will take some time before you start to remember the literally hundreds of key combinations that are basic Emacs commands. Until you do (if ever) you may find an Emacs Reference Card (or "cheat sheet") helpful. A Emacs cheat sheet summarizes the key-combinations for commonly used Emacs commands. A Reference Card can be downloaded [here](https://www.gnu.org/software/emacs/refcards/pdf/refcard.pdf). You can also read the Emacs manual using ```C-h m```. If you are serious about mastering Emacs you should try to use it daily (ideally for most or all of your work).

## Emacs Modes

Emacs modes are libraries of lisp code that extend or modify the behavior of Emacs. There are major and minor modes. Only one major mode can be active in any buffer but several minor modes can be active simultaneously. Many of the major modes provide syntax highlighting and other features for programming languages such as C or Python. Others provide specific functionalities, such as spell checking. Emacs modes are usually distributed as packages. Some packages are installed by default, others must be installed by the user. Certain file suffixes will automatically trigger a mode in Emacs. However, a mode can also be initialized manually using ```M-x mode-name```. You can not turn-off a major mode once it is active because every buffer has to have exactly one major mode and turning off the current major mode would leave the buffer with no major mode. The correct thing to do is to switch to another mode. You can view the documentation for the major mode of the current buffer by typing ```C-h m```. 

### Exercise 2

Create a new file called myfile.txt using the command
```
C-x f myfile.txt
```
Type a few lines of text into the file and save it using ```C-x s```. Then examine the mode that is being used by using the command ```C-h m```. This will open up a second window with the help file for the major mode. Switch to the other window using ```C-x o```. To close the help buffer type ```q``` when in the help buffer. 

## Windows in Emacs

You can have multiple Emacs windows open in a single terminal and the windows can contain different buffers. This allows you to multitask and there are key commands for switching between buffers. To split a window horizontally type ```C-x 2``` and to split vertically type ```C-x 3```. The cursor is in the active window. The command ```C-x o``` switches the active window. Any commands you type apply only to the active window.

### Exercise 3 
Using the command ```C-x 2``` split the current window. Switch to the second window using ```C-x o```. Create a new file
```
C-x f myfile2.txt
```
Enter some text into the file. Then move to the other window and enter some more text into file test.txt. Save the changes in both files using ```C-x s```. Close the file in the active buffer using ```C-x  k```.
