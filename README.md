# swagger-build-mode.el

[![MELPA](https://melpa.org/packages/swagger-build-mode-badge.svg)](https://melpa.org/#/swagger-build-mode)

**swagger-build-mode** is a delightful minor mode for Emacs, crafted to supercharge your workflow when building and editing Swagger (OpenAPI) API definition YAML files. It brings a suite of smart editing tools and convenience features, so you can focus on designing great APIs—right from the comfort of your favorite editor.

## ✨ Features

- Syntax highlighting tailored for Swagger/OpenAPI YAML files
- Effortless navigation of API endpoints and definitions
- Convenient insertion of Swagger components and templates
- Smart utilities to reduce boilerplate and accelerate spec authoring
- Future-proof: hooks for validation and integration with external tools (coming soon!)

## 🚀 Installation

Clone this repo and add it to your Emacs `load-path`:

```elisp
(add-to-list 'load-path "/path/to/swagger-build-mode.el")
(require 'swagger-build-mode)
```

## 🛠 Usage

Open your Swagger or OpenAPI YAML file, then enable the minor mode:

```
M-x swagger-build-mode
```

To activate automatically for `.yaml` or `.yml` files:

```elisp
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . swagger-build-mode))
```

## 🤝 Contributing

All contributions are welcome! If you have ideas, bug fixes, or feature requests, open an issue or submit a pull request. Let’s make building APIs in Emacs a delightful experience together.

## 📝 License

This project is licensed under the GNU General Public License v3.0 (GPL-3.0).  
See [LICENSE](LICENSE) for details.

---

Unleash your API design superpowers with Emacs and swagger-build-mode! 🚀✨
