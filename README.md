# Description
David Jung's Emacs Confiuration.

The main change from the last version.  
- Introduced straight.el and use-package combo.

# Tested
WSL 2 + Ubuntu 20.04 (Focal Fossa) + X Server (Xming, VcXsrv, GWSL, MobaXterm, X410)  
macOS 11.7 (Big Sur)  

# TODOs
1. Upgrade Emacs to 29.1 and tune the overall configuration
2. Fix eshell-mode-hook to work
3. Fix realtime-Markdown-preview to work
4. Moderate vterm faces or change the theme
5. Don't let vterm head to the bottom line automatically
6. Fix Eglot to behave good in C++
7. Let vterm follow along the current directory
8. Fix C-x C-b to locate previous buffer at the top
9. Fix conda environment from being disabled sometimes in eshell
10. Fix Eglot not to switch to base environment when python files connect to Eglot
11. Add sticky function name
12. Transfer global-set-key -> local-set-key for some pakcages
13. Make Helm child frame be on some managed candidate positions
