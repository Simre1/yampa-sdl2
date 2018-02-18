# YampaSDL2

YampaSDL2 provides SDL2 bindings to use with the FRP library [Yampa](https://github.com/ivanperez-keera/Yampa).

![Screenshot](./screenshot.png)
A screenshot of some drawn shapes.

## Roadmap

- [ ] Display Shapes
  - [x] Rectangle
  - [x] Circle
  - [x] Triangle
  - [ ] Polygon
- [ ] Display Images
- [x] Animations
- [x] Handle Input
- [ ] Play sound

**This library is still work in progress**


## Getting started

### Prerequisites

yampa-sdl2 uses the C-libraries
- _sdl2_
- _sdl2-gfx_

Consequently, you need to have these two libraries installed on your computer.

### Installation

1. Download the library:
```bash
cd example-project
git clone https://github.com/Simre1/yampa-sdl2.git
```
2. Add it to your dependencies:
Edit your stack.yaml file and add yampa-sdl2 to packages.
Add the yampa-sdl2 dependency to your cabal file (or package.yaml)

3. Import the library with: `import YampaSDL2`

## How to use

Head over to the [wiki](https://github.com/Simre1/yampa-sdl2/wiki).

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
