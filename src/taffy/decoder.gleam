import gleam/dynamic
import gleam/list
import taffy/value

pub fn yaml_to_dynamic(yaml: value.YamlValue) -> dynamic.Dynamic {
  case yaml {
    value.Null -> dynamic.nil()
    value.Bool(value) -> dynamic.bool(value)
    value.Int(value) -> dynamic.int(value)
    value.Float(value) -> dynamic.float(value)
    value.String(value) -> dynamic.string(value)
    value.Sequence(value) -> list.map(value, yaml_to_dynamic) |> dynamic.list
    value.Mapping(value) ->
      list.map(value, pair_to_dynamics) |> dynamic.properties
  }
}

pub fn pair_to_dynamics(
  value: #(String, value.YamlValue),
) -> #(dynamic.Dynamic, dynamic.Dynamic) {
  let first = dynamic.string(value.0)
  let second = yaml_to_dynamic(value.1)
  #(first, second)
}
