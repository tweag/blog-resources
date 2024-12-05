import XCTest
import SwiftTreeSitter
import TreeSitterYolo

final class TreeSitterYoloTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_yolo())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Yolo grammar")
    }
}
