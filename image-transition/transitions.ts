import { Skia } from "@shopify/react-native-skia";

export const source = Skia.RuntimeEffect.Make(`
uniform shader image1;
uniform shader image2;

uniform float2 resolution;
uniform float progress;

half4 getFromColor(float2 uv) {
    return image1.eval(uv * resolution);
}

half4 getToColor(float2 uv) {
    return image2.eval(uv * resolution);
}

// Author: pschroen
// License: MIT

const vec2 direction = vec2(-1.0, 1.0);

const float smoothness = 0.5;
const vec2 center = vec2(0.5, 0.5);

vec4 transition (vec2 uv) {
  vec2 v = normalize(direction);
  v /= abs(v.x) + abs(v.y);
  float d = v.x * center.x + v.y * center.y;
  float m = 1.0 - smoothstep(-smoothness, 0.0, v.x * uv.x + v.y * uv.y - (d - 0.5 + progress * (1.0 + smoothness)));
  return mix(getFromColor((uv - 0.5) * (1.0 - m) + 0.5), getToColor((uv - 0.5) * m + 0.5), m);
}


half4 main(vec2 xy) {
    vec2 uv = xy / resolution;
    return transition(uv);
  }`)!;

if (!source) {
  throw new Error("Couldn't compile the shader")
}