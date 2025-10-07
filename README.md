




# Keybindings

## gtd

    C-c g             gtd
    C-c r             r&d
    C-c G             game dev

## misc

    C-c d             delete everything after point
    C-c D             delete everything before point
    C-c l             jump to slime
    C-c M-i           fuzzy complete symbol
    

## necromancer (prototype)

    C-c n              cmd
    C-c N              prompt
    C-c C-c <r>        command

## gptel.el

    C-c C-g m         set model
    C-c C-g s         set system prompt
    C-c C-g c         add region to context
    C-c C-g i         inspect 
    C-c C-g x         reset (here)
    C-c C-g X         reset (all)


old (to be archived)
```
;; C-c g          gptel chat
;; C-c RET        send
;; C-c C-g a      add region
;; C-c C-g r      rewrite                      << requires context
;; C-c C-g x      remove context               (X for reset all]
;; C-c C-g m      menu
```

Need:
- add file to context


## copilot

    C-c n        next completion
    C-c p        previous completion
    C-c DEL      delete
    C-c w        accept by word
    C-c l        accept by line
    C-c TAB      accept completion
    C-c RET      accept completion

## markdown

    C-c C-x      toggle strikethrough
    C-c C-RET    touch file (trigger obsidian sync)



## Reference

### auto revert / refreshing te buffer after file changes

     M-x auto-revert-mode
     C-x C-v
     M-x revert-buffer
     remove-overlays


### registers & deleting

    C-x r w <r>    save window config to <r>    (j to jump back)
    C-x h          mark whole buffer
    C-x r s <r>    store region in <r>          (i to insert)
    C-c C-g d      delete after point [D for whole file]
    M-x append-to-register RET r
    M-x prepend-to-register RET r
