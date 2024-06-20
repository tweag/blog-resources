import { Canvas, Fill, ImageShader, Shader } from "@shopify/react-native-skia";
import { Dimensions, StyleSheet, View, Pressable, Alert } from "react-native";
import { AntDesign } from "@expo/vector-icons";
import { useAssets } from "./useAssets";
import { source } from "./transitions";
import {
  runOnJS,
  useDerivedValue,
  useSharedValue,
  withTiming,
} from "react-native-reanimated";
import { useCallback } from "react";

export default function App() {
  const offset = useSharedValue(0);
  const progressPrev = useSharedValue(1);
  const progressNext = useSharedValue(0);
  const isNext = useSharedValue(false);


  const assets = useAssets();
  const { width, height } = Dimensions.get("window");

  const updateNext = useCallback((param: boolean) => {

    isNext.value = param;
  }, []);

  const next = useCallback(() => {
    offset.value += 1;
    progressNext.value = 0;
  }, []);

  const prev = useCallback(() => {
    offset.value -= 1;
    progressPrev.value = 1;
  }, []);

  const handleClickNext = () => {
   
    updateNext(true)
    progressNext.value = withTiming(
      1,
      {
        duration: 1000,
      },
      () => {
        runOnJS(next)();
      }
    );
  };

  const handleClickPrevious = () => {
    updateNext(false)
    progressPrev.value = withTiming(
      0,
      {
        duration: 1000,
      },
      () => {
        runOnJS(prev)();
      }
    );
  };

  const getAsset = useCallback(
    (index: number) => {
      "worklet";
      if (assets === null) {
        return null;
      }
      return assets[((index % assets.length) + assets.length) % assets.length];
    },
    [assets]
  );

  const progress = useDerivedValue(() => {
    return isNext.value ? progressNext.value : progressPrev.value;
  });

  const uniform = useDerivedValue(() => {
    return {
      progress: progress.value,
      resolution: [width, height],
    };
  });

  const image1 = useDerivedValue(() => {
    return getAsset(isNext.value ? offset.value : offset.value - 1);
  });

  const image2 = useDerivedValue(() => {
    return getAsset(isNext.value ? offset.value + 1 : offset.value);
  });
  if (!assets) {
    return null;
  }
  return (
    <View style={styles.container}>
      <View style={styles.buttons}>
        <Pressable onPress={handleClickPrevious} disabled={progressPrev.value > 0 && progressPrev.value< 1} style={styles.button}>
          <AntDesign name="left" size={24} color="black" />
        </Pressable>
        <Pressable onPress={handleClickNext} style={styles.button}>
          <AntDesign name="right" size={24} color="black" />
        </Pressable>
      </View>

      <Canvas style={styles.container}>
        <Fill>
          <Shader source={source} uniforms={uniform}>
            <ImageShader
              fit="cover"
              image={image1}
              width={width}
              height={height}
            />

            <ImageShader
              fit="cover"
              image={image2}
              width={width}
              height={height}
            />
          </Shader>
        </Fill>
      </Canvas>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
  },
  button: {
    backgroundColor: "white",
    alignItems: "center",
    justifyContent: "center",
    padding: 10,
    borderRadius: 16,
  },
  buttons: {
    position: "absolute",
    bottom: 80,
    right: 40,
    zIndex: 10,
    flexDirection: "row",
    gap: 16,
  },
});