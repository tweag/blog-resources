import { useImage } from "@shopify/react-native-skia";

export const useAssets = () => {
  const image1 = useImage(
    "https://images.unsplash.com/photo-1586023492125-27b2c045efd7"
  );
  const image2 = useImage(
    "https://images.unsplash.com/photo-1606744837616-56c9a5c6a6eb"
  );
  const image3 = useImage(
    "https://images.unsplash.com/photo-1502005229762-cf1b2da7c5d6"
  );

  if (!image1 || !image2 || !image3) {
    return null;
  }

  return [image1, image2, image3]
};
