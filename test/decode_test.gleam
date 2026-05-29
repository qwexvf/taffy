import gleam/dynamic/decode
import gleam/json
import taffy

const yaml_to_test = "field1: 9
field2: Hello, Lucy!
field3: true
field4: 4.5
field5:
  field1: \"Items\"
  field2:
    - 1
    - 2
    - 3"

type DecodeTestType {
  DecodeTestType(
    field1: Int,
    field2: String,
    field3: Bool,
    field4: Float,
    field5: DecodeTestSubType,
  )
}

type DecodeTestSubType {
  DecodeTestSubType(field1: String, field2: List(Int))
}

pub fn compare_with_json_test() {
  // yaml string -> yaml -> json string -> decoded
  let assert Ok(yaml) = taffy.parse(yaml_to_test)
  let json_string = echo taffy.to_json_string(yaml)
  let assert Ok(json_decoded) =
    json.parse(json_string, decode_test_type_decoder())

  // yaml string -> decoded
  let assert Ok(yaml_decoded) =
    taffy.decode(yaml_to_test, decode_test_type_decoder())

  // ensure direct decoding results the same as going through json
  assert json_decoded == yaml_decoded
}

fn decode_test_type_decoder() -> decode.Decoder(DecodeTestType) {
  use field1 <- decode.field("field1", decode.int)
  use field2 <- decode.field("field2", decode.string)
  use field3 <- decode.field("field3", decode.bool)
  use field4 <- decode.field("field4", decode.float)
  use field5 <- decode.field("field5", decode_test_sub_type_decoder())
  decode.success(DecodeTestType(field1:, field2:, field3:, field4:, field5:))
}

fn decode_test_sub_type_decoder() -> decode.Decoder(DecodeTestSubType) {
  use field1 <- decode.field("field1", decode.string)
  use field2 <- decode.field("field2", decode.list(decode.int))
  decode.success(DecodeTestSubType(field1:, field2:))
}
