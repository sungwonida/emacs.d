# Description

David Jung's Emacs configuration.

* Uses **straight.el** + **use-package** for reproducible, declarative package management.

# Tested Platforms

* **WSL 2** + **Ubuntu 20.04** (Focal Fossa) with Xming, VcXsrv, GWSL, MobaXterm, X410, WSLg
* **macOS** 11.7 (Big Sur) → 15 (Sequoia)

# TODO

1. Ensure `vterm` stays in sync with the shell’s CWD.
2. Make `C‑x C‑b` open the previously visited buffer by default.
3. Move global key binds to mode hooks where appropriate.
4. Keep Helm child‑frames aligned with candidate lists.

## Changelog

### 2025

* **June**

  * Persistent keyboard‑macro store & WSL batch‑launch helpers.
  * ⌥ → Super mapping on macOS; visual indentation guides in Python.

* **May**

  * GPTel integration + helpers; Helpful & `command‑log‑mode` enabled.
  * Human‑readable sizes in Dired; Helm UX & impatient‑mode fixes.

* **April**

  * Reduced LSP latency; global `kill‑buffer`; removed straight/use‑package cycle.
  * Added `ellama`, `ztree`; centered Helm frame; disabled large‑file warnings.

### 2024

* **Aug** — Replaced `projectile‑ag` with `helm‑ag`; smoother mouse scrolling.
* **Jul** — Treemacs & LSP‑Treemacs; Projectile; TRAMP SSH control; UI polish.

### Earlier

* **StradVision 2.1** (2023‑04‑18) — README update.
* **StradVision 2.0** (2023‑04‑17) — Adopted **straight.el** + **use‑package**.
* **StradVision 1.0** (2022‑06‑30) — VSCode dark theme.
