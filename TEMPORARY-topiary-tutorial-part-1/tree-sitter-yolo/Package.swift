// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "TreeSitterYolo",
    products: [
        .library(name: "TreeSitterYolo", targets: ["TreeSitterYolo"]),
    ],
    dependencies: [
        .package(url: "https://github.com/ChimeHQ/SwiftTreeSitter", from: "0.8.0"),
    ],
    targets: [
        .target(
            name: "TreeSitterYolo",
            dependencies: [],
            path: ".",
            sources: [
                "src/parser.c",
                // NOTE: if your language has an external scanner, add it here.
            ],
            resources: [
                .copy("queries")
            ],
            publicHeadersPath: "bindings/swift",
            cSettings: [.headerSearchPath("src")]
        ),
        .testTarget(
            name: "TreeSitterYoloTests",
            dependencies: [
                "SwiftTreeSitter",
                "TreeSitterYolo",
            ],
            path: "bindings/swift/TreeSitterYoloTests"
        )
    ],
    cLanguageStandard: .c11
)
