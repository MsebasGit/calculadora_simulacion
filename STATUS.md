# Estado del Proyecto - Calculadora de Simulación (WASM)

Este archivo contiene el resumen del estado del proyecto para continuar con el desarrollo de forma rápida.

## Resumen del Refactor (Completado)
* **Objetivo:** Migrar la aplicación Miso de la interfaz antigua/crashed a una ejecución nativa en WebAssembly (WASM).
* **Eliminación de JSaddle:** Se eliminó por completo `jsaddle` y `jsaddle-warp` del proyecto.
* **Miso 1.11.0.0:** Se utiliza la última versión de Miso que prescinde de JSaddle.
* **Compatibilidad de compilación nativa:** El punto de entrada en `app/Main.hs` está protegido con macros `#ifdef WASM`. Al compilar de forma nativa (`cabal build`), el programa compila pero no crashea en tiempo de ejecución (imprime las instrucciones de WASM en consola).

---

## Entorno de Compilación
* **Flavour de GHC WASM:** GHC 9.12 (`FLAVOUR=9.12`).
* **Ubicación de la Toolchain:** `$HOME/.ghc-wasm/`

### Cargar el entorno (Terminal):
* **Si usas Fish Shell (Recomendado):**
  ```fish
  source ~/.ghc-wasm/env.fish
  ```
* **Si usas Bash Shell:**
  ```bash
  source ~/.ghc-wasm/env
  ```

---

## Comandos Rápidos de Desarrollo

1. **Compilar y Empaquetar a WebAssembly:**
   Ejecuta el script automatizado que se encuentra en la raíz del proyecto. Este script compilará, ejecutará el post-linker para los bindings JS y copiará todo a la carpeta `public/`:
   ```bash
   ./build-wasm.sh
   ```

2. **Iniciar Servidor Local de Desarrollo:**
   Dado que los archivos WASM requieren ser servidos por HTTP, levanta un servidor de desarrollo con Python en la carpeta `public`:
   ```bash
   python3 -m http.server -d public 8080
   ```

3. **Abrir en el Navegador:**
   Navega a: [http://localhost:8080](http://localhost:8080)

---

## Archivos Clave del Proyecto
* [app/Main.hs](file:///home/bass/Documents/universidad/semestre6/SimulaGOD/calculadora_simulacion/app/Main.hs): Punto de entrada condicional WASM.
* [build-wasm.sh](file:///home/bass/Documents/universidad/semestre6/SimulaGOD/calculadora_simulacion/build-wasm.sh): Script de automatización de compilación WASM.
* [public/index.html](file:///home/bass/Documents/universidad/semestre6/SimulaGOD/calculadora_simulacion/public/index.html): Plantilla con el cargador de WASI y knot-tying para JS FFI.
