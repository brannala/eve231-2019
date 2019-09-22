## Using Git to Maintain Your Files
The git program allows you to retain copies of all previous and current versions of your files without any clutter in your project directory. To start using git first create a directory for your project files
```
mkdir myproject
cd my project
```
then create a new git repository in this directory
```
git init
```
This has created a git revision control system. All the files used by git are in a hidden subdirectory called .git and can be viewed using 
```
tree .git
```
If you want to change the directory back to a non-git directory simply delete the subdirectory .git using a command like ```rm -Rf .git```.
