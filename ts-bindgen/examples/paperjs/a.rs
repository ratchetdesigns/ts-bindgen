#[cfg(target_family = "wasm")]
pub mod a {
    #[allow(unused)]
    use wasm_bindgen::prelude::*;
    #[wasm_bindgen(module = "a.js")]
    extern "C" {
        #[wasm_bindgen(js_name = "B")]
        pub type B_Class;
        # [wasm_bindgen (method , structural , getter = a , js_class = "B")]
        fn a(this: &B_Class) -> A;
        # [wasm_bindgen (method , structural , setter = a , js_class = "B")]
        fn set_a(this: &B_Class, value: A);
        # [wasm_bindgen (method , structural , getter = n , js_class = "B")]
        fn n(this: &B_Class) -> f64;
        # [wasm_bindgen (method , structural , setter = n , js_class = "B")]
        fn set_n(this: &B_Class, value: f64);
    }
    #[derive(std :: clone :: Clone)]
    pub struct B(pub B_Class);
    impl B {
        #[allow(dead_code)]
        pub fn a(&self) -> A {
            let result = self.0.a();
            result
        }
        #[allow(dead_code)]
        pub fn set_a(&self, value: A) -> () {
            let result = self.0.set_a(value);
            result
        }
        #[allow(dead_code)]
        pub fn n(&self) -> f64 {
            let result = self.0.n();
            result
        }
        #[allow(dead_code)]
        pub fn set_n(&self, value: f64) -> () {
            let result = self.0.set_n(value);
            result
        }
    }
    impl wasm_bindgen::describe::WasmDescribe for B {
        fn describe() {
            <B_Class as wasm_bindgen::describe::WasmDescribe>::describe()
        }
    }
    impl wasm_bindgen::convert::IntoWasmAbi for B {
        type Abi = <B_Class as wasm_bindgen::convert::IntoWasmAbi>::Abi;
        fn into_abi(self) -> Self::Abi {
            wasm_bindgen::convert::IntoWasmAbi::into_abi(self.0)
        }
    }
    impl<'a> wasm_bindgen::convert::IntoWasmAbi for &'a B {
        type Abi = <&'a B_Class as wasm_bindgen::convert::IntoWasmAbi>::Abi;
        fn into_abi(self) -> Self::Abi {
            wasm_bindgen::convert::IntoWasmAbi::into_abi(&self.0)
        }
    }
    impl serde::ser::Serialize for B {
        fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: serde::ser::Serializer,
        {
            serde::ser::Serialize::serialize(&self.0, serializer)
        }
    }
    impl<'de> serde::de::Deserialize<'de> for B {
        fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
        where
            D: serde::de::Deserializer<'de>,
        {
            let internal: B_Class = <B_Class as serde::de::Deserialize>::deserialize(deserializer)?;
            std::result::Result::Ok(Self(internal))
        }
    }
    #[allow(non_camel_case_types)]
    pub trait B_Trait {
        fn a(&self) -> A;
        fn set_a(&mut self, value: A) -> ();
        fn n(&self) -> f64;
        fn set_n(&mut self, value: f64) -> ();
    }
    impl std::clone::Clone for B_Class {
        fn clone(&self) -> Self {
            Self {
                obj: std::clone::Clone::clone(&self.obj),
            }
        }
    }
    impl serde::ser::Serialize for B_Class {
        fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: serde::ser::Serializer,
        {
            ts_bindgen_rt::serialize_as_jsvalue(serializer, self)
        }
    }
    impl<'de> serde::de::Deserialize<'de> for B_Class {
        fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
        where
            D: serde::de::Deserializer<'de>,
        {
            ts_bindgen_rt::deserialize_as_jsvalue(deserializer)
        }
    }
    #[wasm_bindgen(module = "a.js")]
    extern "C" {
        #[wasm_bindgen(js_name = "A")]
        pub type A_Class;
    }
    #[derive(std :: clone :: Clone)]
    pub struct A(pub A_Class);
    impl A {}
    impl wasm_bindgen::describe::WasmDescribe for A {
        fn describe() {
            <A_Class as wasm_bindgen::describe::WasmDescribe>::describe()
        }
    }
    impl wasm_bindgen::convert::IntoWasmAbi for A {
        type Abi = <A_Class as wasm_bindgen::convert::IntoWasmAbi>::Abi;
        fn into_abi(self) -> Self::Abi {
            wasm_bindgen::convert::IntoWasmAbi::into_abi(self.0)
        }
    }
    impl<'a> wasm_bindgen::convert::IntoWasmAbi for &'a A {
        type Abi = <&'a A_Class as wasm_bindgen::convert::IntoWasmAbi>::Abi;
        fn into_abi(self) -> Self::Abi {
            wasm_bindgen::convert::IntoWasmAbi::into_abi(&self.0)
        }
    }
    impl serde::ser::Serialize for A {
        fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: serde::ser::Serializer,
        {
            serde::ser::Serialize::serialize(&self.0, serializer)
        }
    }
    impl<'de> serde::de::Deserialize<'de> for A {
        fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
        where
            D: serde::de::Deserializer<'de>,
        {
            let internal: A_Class = <A_Class as serde::de::Deserialize>::deserialize(deserializer)?;
            std::result::Result::Ok(Self(internal))
        }
    }
    #[allow(non_camel_case_types)]
    pub trait A_Trait {}
    impl std::clone::Clone for A_Class {
        fn clone(&self) -> Self {
            Self {
                obj: std::clone::Clone::clone(&self.obj),
            }
        }
    }
    impl serde::ser::Serialize for A_Class {
        fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: serde::ser::Serializer,
        {
            ts_bindgen_rt::serialize_as_jsvalue(serializer, self)
        }
    }
    impl<'de> serde::de::Deserialize<'de> for A_Class {
        fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
        where
            D: serde::de::Deserializer<'de>,
        {
            ts_bindgen_rt::deserialize_as_jsvalue(deserializer)
        }
    }
}
