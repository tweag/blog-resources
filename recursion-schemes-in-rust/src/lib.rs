use std::collections::HashMap;

mod span {
    pub type Span = std::ops::Range<usize>;

    pub struct Spanned<T> {
        pos: Span,
        data: T,
    }

    pub type SpannedValue = Spanned<JsonValue>;

    pub enum JsonValue {
        String(String),
        Number(f64),
        Pair(Box<SpannedValue>, Box<SpannedValue>),
        Array(Vec<SpannedValue>),
        Object(super::HashMap<String, SpannedValue>),
    }
}

pub struct JsonValue {
    data: JsonValueF<Box<JsonValue>>,
}

pub enum JsonValueF<T> {
    String(String),
    Number(f64),
    Pair(T, T),
    Array(Vec<T>),
    Object(HashMap<String, T>),
}

/// A version of [JsonValueF] that borrows the data in the leafs. This is useful for deriving a
/// borrowed version of `&JsonValueF<T>` without cloning the data in the leafs (versus the other
/// contender `JsonValueF<&T>`).
pub enum JsonValueRefF<'a, T> {
    String(&'a String),
    Number(&'a f64),
    Pair(T, T),
    Array(Vec<T>),
    Object(HashMap<&'a str, T>),
}

pub type JsonValueRef<'a, T> = JsonValueRefF<'a, &'a T>;

pub struct SpannedJsonValue {
    data: JsonValueF<Box<SpannedJsonValue>>,
    span: std::ops::Range<u32>,
}

pub struct SharedJsonValue {
    data: JsonValueF<std::rc::Rc<JsonValue>>,
}

pub struct ArenaJsonValue<'a> {
    data: JsonValueF<&'a JsonValue>,
}

pub fn count_strings_naive(value: &JsonValue) -> u32 {
    match &value.data {
        JsonValueF::String(_) => 1,
        JsonValueF::Number(_) => 0,
        JsonValueF::Pair(first, second) => count_strings_naive(first) + count_strings_naive(second),
        JsonValueF::Array(array) => array.iter().map(|elt| count_strings_naive(elt)).sum(),
        JsonValueF::Object(object) => object.values().map(|elt| count_strings_naive(elt)).sum(),
    }
}

pub fn count_strings_map(value: JsonValue) -> u32 {
    match value.data.map(|unr| count_strings_map(*unr)) {
        JsonValueF::String(_) => 1,
        JsonValueF::Number(_) => 0,
        JsonValueF::Pair(first, second) => first + second,
        JsonValueF::Array(array) => array.iter().sum(),
        JsonValueF::Object(object) => object.values().sum(),
    }
}

pub fn count_strings_ref(value: &JsonValue) -> u32 {
    value.fold(&mut |unr| match unr {
        JsonValueRefF::String(_) => 1,
        JsonValueRefF::Number(_) => 0,
        JsonValueRefF::Pair(first, second) => first + second,
        JsonValueRefF::Array(array) => array.iter().sum(),
        JsonValueRefF::Object(object) => object.values().sum(),
    }) 
}

impl<T> JsonValueF<T> {
    pub fn map<U>(self, mut f: impl FnMut(T) -> U) -> JsonValueF<U> {
        match self {
            JsonValueF::String(s) => JsonValueF::String(s),
            JsonValueF::Number(n) => JsonValueF::Number(n),
            JsonValueF::Pair(first, second) => JsonValueF::Pair(f(first), f(second)),
            JsonValueF::Array(array) => {
                JsonValueF::Array(array.into_iter().map(|elt| f(elt)).collect())
            }
            JsonValueF::Object(object) => {
                JsonValueF::Object(object.into_iter().map(|(k, v)| (k, f(v))).collect())
            }
        }
    }

    /// Same as [Self::map], but takes its argument by reference. However, we don't want to clone
    /// strings and numbers in the output, so instead of [Self], we return a variant that also
    /// borrows the data in the leafs.
    pub fn map_ref<U>(&self, mut f: impl FnMut(&T) -> U) -> JsonValueRefF<U> {
        match self {
            JsonValueF::String(s) => JsonValueRefF::String(s),
            JsonValueF::Number(n) => JsonValueRefF::Number(n),
            JsonValueF::Pair(first, second) => JsonValueRefF::Pair(f(first), f(second)),
            JsonValueF::Array(array) => {
                JsonValueRefF::Array(array.into_iter().map(|elt| f(elt)).collect())
            }
            JsonValueF::Object(object) => {
                JsonValueRefF::Object(object.into_iter().map(|(k, v)| (k.as_str(), f(v))).collect())
            }
        }
    }

}

impl JsonValue {
    pub fn map_bottom_up(self: JsonValue, f: &mut impl FnMut(JsonValue) -> JsonValue) -> JsonValue {
        let data = self.data.map(|v| Box::new(v.map_bottom_up(f)));
        f(JsonValue { data })
    }

    /// A fold function, similar to [Iterator::fold], but for JSON values. From a way to generate
    /// values of type `A` from the leafs, and a way to combine values of type `A` for each node
    /// with children into one value of type `A`, we can compute a value of type `A` for the whole
    /// JSON value.
    ///
    /// Another common presentation, instead of taking one function from `JsonValueRefF<A>` as
    /// an argument, is to take one function per variant: `fn string(&str) -> A`, `fn number(&f64)
    /// -> A`, `fn object(&HashMap<&str, A>) -> A`, etc. The two are presentations equivalent.
    pub fn fold<A>(
        &self,
        f: &mut impl FnMut(JsonValueRefF<A>) -> A,
    ) -> A {
        let unrolled = self.data.map_ref(|unr| unr.fold(f));
        f(unrolled)
    }
}
