//! This is helper crate intended to be used internally in nop_json crate.
//! Don't use it directly. See nop_json crate for details.

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{self, DeriveInput, Data, Attribute, Ident, Meta, NestedMeta, Lit, LitByteStr, LitStr, Generics, ImplGenerics, TypeGenerics, GenericParam, TypeParam};
use std::borrow::Cow;
use std::mem;
use std::collections::{HashMap, HashSet};

const HEX_DIGITS: [u8; 16] = [b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'A', b'B', b'C', b'D', b'E', b'F'];

/// This is copy/paste fn from nop_json
fn escape(s: &str) -> Cow<str>
{	let bytes = s.as_bytes();
	if let Some(mut pos) = bytes.iter().position(|c| match *c {b'"' | b'\\' | 0..=31 => true, _ => false})
	{	let mut buffer = Vec::with_capacity(bytes.len() + 8);
		let mut from = 0;
		loop
		{	buffer.extend_from_slice(&bytes[from .. pos]);
			let c = bytes[pos];
			if c >= 32
			{	buffer.push(b'\\');
				buffer.push(c);
			}
			else
			{	match c
				{	9 =>
					{	buffer.push(b'\\');
						buffer.push(b't');
					}
					13 =>
					{	buffer.push(b'\\');
						buffer.push(b'r');
					}
					10 =>
					{	buffer.push(b'\\');
						buffer.push(b'n');
					}
					8 =>
					{	buffer.push(b'\\');
						buffer.push(b'b');
					}
					12 =>
					{	buffer.push(b'\\');
						buffer.push(b'f');
					}
					_ =>
					{	buffer.push(b'\\');
						buffer.push(b'u');
						buffer.push(b'0');
						buffer.push(b'0');
						buffer.push(HEX_DIGITS[(c >> 4) as usize]);
						buffer.push(HEX_DIGITS[(c & 0xF) as usize]);
					}
				}
			}
			from = pos + 1;
			if let Some(new_pos) = &bytes[from ..].iter().position(|c| match *c {b'"' | b'\\' | 0..=31 => true, _ => false})
			{	pos = from + *new_pos;
			}
			else
			{	buffer.extend_from_slice(&bytes[from .. ]);
				break;
			}
		}
		Cow::Owned(String::from_utf8(buffer).unwrap())
	}
	else
	{	Cow::Borrowed(s)
	}
}

/// To generate TryFromJson implementation for any struct or enum, where all members also implement TryFromJson
/// use `#[derive(TryFromJson)]`.
///
/// See nop_json crate for details.
#[proc_macro_derive(TryFromJson, attributes(json, json_ignore))]
pub fn derive_try_from_json(input: TokenStream) -> TokenStream
{	let ast: &mut DeriveInput = &mut syn::parse(input).unwrap();
	match impl_try_from_json(ast)
	{	Ok(ts) => ts,
		Err(error) =>
		{	panic!(error);
		}
	}
}

fn impl_try_from_json(ast: &mut DeriveInput) -> Result<TokenStream, String>
{	let name = &ast.ident;
	let mut json_ignore = HashMap::new();
	let mut is_ignore_all = get_json_ignore(&ast.attrs, usize::MAX, &mut json_ignore, false)?;
	let mut variants = Vec::new();
	let mut code = quote!();
	let mut code_2 = quote!();
	let mut code_3 = quote!();
	match &ast.data
	{	Data::Struct(data_struct) =>
		{	for field in data_struct.fields.iter()
			{	if let Some(ref field_name) = field.ident
				{	let mut json_str = get_json_name(&field.attrs, "struct field")?.unwrap_or_else(|| field_name.to_string());
					let is_transient = json_str.is_empty();
					if is_transient
					{	json_str = field_name.to_string();
					}
					// code
					if !is_transient
					{	code = quote!( #code let mut #field_name = None; );
					}
					// code_2
					let b = LitByteStr::new(json_str.as_bytes(), Span::call_site());
					if !is_transient
					{	code_2 = quote!( #code_2 #b => #field_name = reader.read_prop(#json_str)?, );
					}
					else
					{	code_2 = quote!( #code_2 #b => {let skip: () = reader.read_prop(#json_str)?;}, );
					}
					// code_3
					if !is_transient
					{	code_3 = quote!( #code_3 #field_name: #field_name.unwrap_or_default(), );
					}
					else
					{	code_3 = quote!( #code_3 #field_name: Default::default(), );
					}
				}
			}
			code_3 = quote!( let result = Self{#code_3} );
		},
		Data::Enum(data_enum) =>
		{	let enum_json_name = get_json_name(&ast.attrs, "enum")?.unwrap_or_default();
			let mut fields = Vec::new();
			let mut fields_by_json_name = HashMap::new();
			// scan variants
			for (n_variant, variant) in data_enum.variants.iter().enumerate()
			{	let variant_name = &variant.ident;
				let (variant_name_str, json_names) = get_json_name_for_enum_variant(&variant.attrs, variant_name, variant.fields.len())?;
				is_ignore_all = get_json_ignore(&variant.attrs, n_variant, &mut json_ignore, is_ignore_all)?;
				let variant_name_str = variant_name_str.unwrap_or_else(|| variant_name.to_string());
				let mut n_field = 0;
				for json_name in &json_names
				{	if !json_name.is_empty() // if not transient
					{	let cur = fields_by_json_name.get_mut(json_name);
						match cur
						{	None =>
							{	fields_by_json_name.insert(json_name.clone(), (Ident::new(&format!("val_{}_{}", variant_name, n_field), Span::call_site()), 1));
							}
							Some(cur) =>
							{	cur.1 += 1;
							}
						}
						let (val_field, n_occurances) = fields_by_json_name.get(json_name).unwrap();
						fields.push((variants.len(), json_name.clone(), val_field.clone(), *n_occurances>1));
						n_field += 1;
					}
					else
					{	fields.push((variants.len(), json_name.clone(), Ident::new("u", Span::call_site()), false));
					}
				}
				variants.push((variant_name, variant_name_str, json_names));
			}
			// scan json_ignore
			let mut enum_variant_cannot_be = HashMap::new();
			for (_json_name, ignore_in_variants) in &json_ignore
			{	if ignore_in_variants.len() != variants.len() && !ignore_in_variants.contains(&usize::MAX)
				{	for (n_variant, (variant_name, _variant_name_str, _json_names)) in variants.iter().enumerate()
					{	if !ignore_in_variants.contains(&n_variant)
						{	let enum_name = LitStr::new(&format!("{}::{}", name, variant_name), Span::call_site());
							let code_c = quote!
							(	if let Some(prop_name) = enum_variant_cannot_be[#n_variant]
								{	return Err(reader.format_error_fmt(format_args!("Field {} is invalid in variant {}", String::from_utf8_lossy(prop_name), #enum_name)));
								}
							);
							enum_variant_cannot_be.insert(n_variant, code_c);
						}
					}
				}
			}
			if !enum_variant_cannot_be.is_empty()
			{	let n_variants = variants.len();
				code = quote!( #code let mut enum_variant_cannot_be: [Option<&[u8]>; #n_variants] = [None; #n_variants]; );
			}
			// form resulting code parts
			let mut code_4 = quote!();
			for (n_variant, (variant_name, variant_name_str, json_names)) in variants.iter().enumerate()
			{	let mut code_5 = quote!();
				let has_fields = !json_names.is_empty();
				for json_name in json_names
				{	if !json_name.is_empty() // if not transient
					{	let (val_field, n_occurances) = fields_by_json_name.get(json_name).unwrap();
						if *n_occurances <= 1
						{	code_5 = quote!( #code_5 #val_field.unwrap_or_default(), );
						}
						else
						{	code_5 = quote!( #code_5 #val_field.try_into().unwrap_or_default(), );
						}
					}
					else
					{	code_5 = quote!( #code_5 Default::default(), );
					}
				}
				let pref_variant_name = Ident::new(&format!("Var{}", variant_name), Span::call_site());
				if !enum_json_name.is_empty()
				{	let b = LitByteStr::new(variant_name_str.as_bytes(), Span::call_site());
					code_2 = quote!( #code_2 #b => EnumVariant::#pref_variant_name, );
					code_3 = quote!( #code_3 #pref_variant_name, );
					if has_fields
					{	code_5 = quote!( (#code_5) );
					}
					let mut code_c = quote!();
					if let Some(code_c_2) = enum_variant_cannot_be.get(&n_variant)
					{	code_c = quote!(#code_c #code_c_2);
					};
					code_4 = quote!( #code_4 EnumVariant::#pref_variant_name => {#code_c Self::#variant_name #code_5}, );
				}
			}
			if !enum_json_name.is_empty()
			{	code = quote!( #code enum EnumVariant {Invalid, #code_3} let mut enum_variant_field = EnumVariant::Invalid; );
				let b = LitByteStr::new(enum_json_name.as_bytes(), Span::call_site());
				code_2 = quote!
				{	#b =>
					{	enum_variant_field = match reader.read_bytes()?
						{	#code_2
							_ => return Err(reader.format_error("Invalid enum variant"))
						};
					},
				};
			}
			//
			let mut code_5 = quote!();
			for (_n_variant, json_name, val_field, is_dup_json_name) in &fields
			{	if !json_name.is_empty() // if not transient
				{	// code
					let (_val_field, n_occurances) = fields_by_json_name.get(json_name).unwrap();
					if *n_occurances <= 1
					{	// is not one of duplicate json names
						code = quote!( #code let mut #val_field = None; );
					}
					else if !*is_dup_json_name
					{	// is first occurance of duplicate json name
						code = quote!( #code let mut #val_field = nop_json::Value::Null; );
					}
					// code_2
					if !*is_dup_json_name
					{	let b = LitByteStr::new(json_name.as_bytes(), Span::call_site());
						code_2 = quote!( #code_2 #b => #val_field = reader.read_prop(#json_name)?, );
					}
					// code_5
					code_5 = quote!( #code_5 #val_field, );
				}
			}
			if !enum_json_name.is_empty()
			{	code_3 = quote!
				{	let result = match enum_variant_field
					{	EnumVariant::Invalid => return Err(reader.format_error(concat!("Field ", #enum_json_name, " is required"))),
						#code_4
					}
				};
			}
			else
			{	for n_variant in 0..data_enum.variants.len()
				{	let mut code_6 = quote!();
					let mut code_7 = quote!();
					let mut has_fields = false;
					for (field_n_variant, json_name, val_field, _is_dup_json_name) in &fields
					{	if *field_n_variant == n_variant
						{	if !json_name.is_empty() // if not transient
							{	code_6 = quote!( #code_6 Some(#val_field), );
								code_7 = quote!( #code_7 #val_field, );
							}
							else
							{	code_7 = quote!( #code_7 Default::default(), );
							}
							has_fields = true;
						}
						else if !json_name.is_empty() // if not transient
						{	code_6 = quote!( #code_6 None, );
						}
					}
					if has_fields
					{	code_7 = quote!( (#code_7) );
					}
					let variant_name = &data_enum.variants[n_variant].ident;
					let mut code_c = quote!();
					if let Some(code_c_2) = enum_variant_cannot_be.get(&n_variant)
					{	code_c = quote!(#code_c #code_c_2);
					};
					code_3 = quote!( #code_3 (#code_6) => {#code_c Self::#variant_name #code_7}, );
				}
				code_3 = quote!( let result = match (#code_5) { #code_3 _ => return Err(reader.format_error("Invalid combination of properties"))} );
			}
		},
		Data::Union(_data_union) =>
		{	return Err("Cannot deserialize union".to_string());
		},
	};
	// ignore?
	let code_8 = if is_ignore_all
	{	// ignore all
		quote!( _ => { reader.read::<()>()? })
	}
	else
	{	let mut code_8 = quote!();
		// ignore only names from "json_ignore"
		for (json_name, ignore_in_variants) in json_ignore
		{	let b = LitByteStr::new(json_name.as_bytes(), Span::call_site());
			let mut code_9 = quote!();
			if ignore_in_variants.len() != variants.len() && !ignore_in_variants.contains(&usize::MAX)
			{	for n in 0..variants.len()
				{	if !ignore_in_variants.contains(&n)
					{	code_9 = quote!( #code_9 enum_variant_cannot_be[#n] = Some(#b); );
					}
				}
			}
			code_8 = quote!( #code_8 #b => { reader.read::<()>()?; #code_9 }, );
		}
		quote!( #code_8 _ => {return Err(reader.format_error_fmt(format_args!("Invalid property: {}", String::from_utf8_lossy(reader.get_key()))))} )
	};
	// get generic parameters of this type (like struct<T> {...})
	let (impl_generics, ty_generics, where_clause) = get_generics_debug_to_json(&ast.generics);
	code = quote!
	{	impl #impl_generics nop_json::TryFromJson for #name #ty_generics #where_clause
		{	fn try_from_json<T>(reader: &mut nop_json::Reader<T>) -> std::io::Result<Self> where T: Iterator<Item=u8>
			{	use nop_json::ValidateJson;
				use std::convert::TryInto;
				#code
				reader.read_object_use_buffer
				(	|reader|
					{	match reader.get_key()
						{	#code_2
							#code_8
						}
						Ok(())
					}
				)?;
				#code_3;
				result.validate_json().map_err(|msg| reader.format_error(&msg))
			}
		}
	};
	// to see what i produced, uncomment the panic!() below, and try to compile your code with #[derive(TryFromJson)]
//panic!(code.to_string());
	// done
	Ok(code.into())
}


/// To generate DebugToJson implementation for any struct or enum, where all members also implement DebugToJson
/// use `#[derive(DebugToJson)]`.
///
/// See nop_json crate for details.
#[proc_macro_derive(DebugToJson, attributes(json))]
pub fn derive_debug_to_json(input: TokenStream) -> TokenStream
{	let ast: &mut DeriveInput = &mut syn::parse(input).unwrap();
	match impl_debug_or_write_to_json(ast, false)
	{	Ok(ts) => ts,
		Err(error) =>
		{	panic!(error);
		}
	}
}

fn impl_debug_or_write_to_json(ast: &mut DeriveInput, is_write_to_json: bool) -> Result<TokenStream, String>
{	let name = &ast.ident; // struct or enum name
	let mut code = quote!();
	match &ast.data
	{	Data::Struct(data_struct) =>
		{	let mut n_field = 0;
			for field in data_struct.fields.iter()
			{	if let Some(ref field_name) = field.ident
				{	let json_str = get_json_name(&field.attrs, "struct field")?.unwrap_or_else(|| field_name.to_string());
					if !json_str.is_empty() // if not transient
					{	let fmt = if n_field == 0
						{	format!("{{{{\"{}\":", escape(&json_str))
						}
						else
						{	format!(",\"{}\":", escape(&json_str))
						};
						code = if !is_write_to_json
						{	quote!( #code write!(out, #fmt)?; nop_json::DebugToJson::fmt(&self.#field_name, out)?; )
						}
						else
						{	quote!( #code write!(out, #fmt)?; nop_json::WriteToJson::write_to_json(&self.#field_name, out)?; )
						};
						n_field += 1;
					}
				}
			}
			if n_field == 0
			{	code = quote!( #code write!(out, "{{}}") );
			}
			else
			{	code = quote!( #code write!(out, "}}") );
			}
		},
		Data::Enum(data_enum) =>
		{	let enum_json_name = get_json_name(&ast.attrs, "enum")?.unwrap_or_default();
			for variant in &data_enum.variants
			{	let variant_name = &variant.ident;
				let (variant_name_str, json_names) = get_json_name_for_enum_variant(&variant.attrs, variant_name, variant.fields.len())?;
				let mut n_field = 0;
				let mut has_named_fields = false;
				let mut code_2 = quote!();
				let mut code_3 = quote!();
				if !enum_json_name.is_empty()
				{	let variant_name_str = variant_name_str.unwrap_or_else(|| variant_name.to_string());
					let fmt = format!("{{{{\"{}\":\"{}\"", escape(&enum_json_name), escape(&variant_name_str));
					code_3 = quote!( #code_3 write!(out, #fmt)?; );
					has_named_fields = true;
				}
				for json_name in json_names
				{	let is_transient = json_name.is_empty();
					// code_2
					let val_field = Ident::new(&format!("{}val_{}", if is_transient {"_"} else {""}, n_field), Span::call_site());
					code_2 = quote!( #code_2 ref #val_field, );
					if !is_transient
					{	// code_3
						let fmt = if n_field==0 && enum_json_name.is_empty()
						{	format!("{{{{\"{}\":", escape(&json_name))
						}
						else
						{	format!(",\"{}\":", escape(&json_name))
						};
						code_3 = if !is_write_to_json
						{	quote!( #code_3 write!(out, #fmt)?; nop_json::DebugToJson::fmt(#val_field, out)?; )
						}
						else
						{	quote!( #code_3 write!(out, #fmt)?; nop_json::WriteToJson::write_to_json(#val_field, out)?; )
						};
						has_named_fields = true;
					}
					//
					n_field += 1;
				}
				if n_field > 0
				{	code_2 = quote!( (#code_2) );
				}
				if !has_named_fields
				{	code_3 = quote!( #code_3 write!(out, "{{")?; );
				}
				code = quote!( #code #name::#variant_name #code_2 => {#code_3} );
			}
			code = quote!( match *self {#code} write!(out, "}}") );
		},
		Data::Union(_data_union) =>
		{	return Err("Cannot serialize union".to_string());
		},
	};
	// produce the impl
	if !is_write_to_json
	{	// get generic parameters of this type (like struct<T> {...})
		let (impl_generics, ty_generics, where_clause) = get_generics_debug_to_json(&ast.generics);
		// impl DebugToJson and impl Debug
		code = quote!
		{	impl #impl_generics nop_json::DebugToJson for #name #ty_generics #where_clause
			{	fn fmt(&self, out: &mut std::fmt::Formatter) -> std::fmt::Result
				{	#code
				}
			}
			impl #impl_generics std::fmt::Debug for #name #ty_generics #where_clause
			{	fn fmt(&self, out: &mut std::fmt::Formatter) -> std::fmt::Result
				{	nop_json::DebugToJson::fmt(self, out)
				}
			}
		};
	}
	else
	{	// get generic parameters of this type (like struct<T> {...})
		let mut generics = mem::take(&mut ast.generics);
		let (impl_generics, ty_generics, where_clause) = get_generics_write_to_json(&mut generics);
		// impl WriteToJson
		code = quote!
		{	impl #impl_generics nop_json::WriteToJson<WriteToJsonPriv1> for #name #ty_generics #where_clause
			{	fn write_to_json(&self, out: &mut WriteToJsonPriv1) -> std::io::Result<()>
				{	#code
				}
			}
		};
	}
	// to see what i produced, uncomment the panic!() below, and try to compile your code with #[derive(DebugToJson)]
//panic!(code.to_string());
	// done
	Ok(code.into())
}

fn get_json_name(attrs: &Vec<Attribute>, what: &str) -> Result<Option<String>, String>
{	let mut result = parse_json_attr(attrs, 0, None)?;
	if result.0.is_some()
	{	return Err(format!("Cannot parse #[json(...)] for {}", what));
	}
	if result.1.len() > 1
	{	return Err(format!("#[json(...)] for {} must contain 1 field name", what));
	}
	Ok(result.1.pop())
}

fn get_json_name_for_enum_variant(attrs: &Vec<Attribute>, variant_name: &Ident, n_fields: usize) -> Result<(Option<String>, Vec<String>), String>
{	parse_json_attr(attrs, n_fields, Some(variant_name))
}

fn parse_json_attr(attrs: &Vec<Attribute>, n_fields: usize, variant_name: Option<&Ident>) -> Result<(Option<String>, Vec<String>), String>
{	parse_json_attr_sub(attrs, n_fields, variant_name.is_some()).map_err
	(	|e|
		if let Some(variant_name) = variant_name
		{	format!("Cannot parse #[json(...)] in enum variant {}{}{}", variant_name, if e.is_empty() {""} else {": "}, e)
		}
		else
		{	format!("Cannot parse #[json(...)]{}{}", if e.is_empty() {""} else {": "}, e)
		}
	)
}

fn parse_json_attr_sub(attrs: &Vec<Attribute>, n_fields: usize, is_enum: bool) -> Result<(Option<String>, Vec<String>), String>
{	let mut group_name = None;
	let mut json_names = Vec::new();
	let mut is_var_str = false;
	for a in attrs
	{	match a.parse_meta()
		{	Ok(Meta::List(list)) =>
			{	if list.path.is_ident("json")
				{	for a in list.nested
					{	match a
						{	NestedMeta::Lit(Lit::Str(s)) =>
							{	if group_name.is_some() && !is_var_str
								{	return Err("Couldn't interpret #[json] attribute".to_string());
								}
								json_names.push(s.value());
							},
							NestedMeta::Meta(Meta::Path(meta)) =>
							{	if group_name.is_some() && !is_var_str
								{	return Err("Couldn't interpret #[json] attribute".to_string());
								}
								if let Some(name) = meta.get_ident()
								{	json_names.push(name.to_string());
								}
							},
							NestedMeta::Meta(Meta::List(list)) =>
							{	if is_enum
								{	if json_names.len() > 0
									{	return Err("Couldn't interpret #[json] attribute".to_string());
									}
									if let Some(name) = list.path.get_ident()
									{	group_name = Some(name.to_string());
										for a in list.nested
										{	match a
											{	NestedMeta::Lit(Lit::Str(s)) =>
												{	json_names.push(s.value());
												},
												NestedMeta::Meta(Meta::Path(meta)) =>
												{	if let Some(name) = meta.get_ident()
													{	json_names.push(name.to_string());
													}
												},
												_ =>
												{	return Err("Couldn't interpret #[json] attribute".to_string());
												}
											}
										}
									}
									if json_names.len() == 0
									{	return Err("Couldn't interpret #[json] attribute".to_string());
									}
								}
								else
								{	return Err("Couldn't interpret #[json] attribute".to_string());
								}
							},
							NestedMeta::Meta(Meta::NameValue(list)) =>
							{	if is_enum
								{	if group_name.is_some()
									{	return Err("Couldn't interpret #[json] attribute".to_string());
									}
									if list.path.is_ident("var")
									{	match list.lit
										{	Lit::Str(s) =>
											{	group_name = Some(s.value());
												is_var_str = true;
											},
											_ => {}
										}
									}
									if group_name.is_none()
									{	return Err("Couldn't interpret #[json] attribute".to_string());
									}
								}
								else
								{	return Err("Couldn't interpret #[json] attribute".to_string());
								}
							},
							_ =>
							{	return Err("Couldn't interpret #[json] attribute".to_string());
							}
						}
					}
				}
			},
			_ => {}
		}
	}
	if is_enum
	{	if json_names.len() != n_fields
		{	if json_names.len()==0
			{	return Err(format!("Enum variant must have #[json(name_1, name_2, ...)] or #[json(variant_name(name_1, name_2, ...))] or #[json(var=\"variant_name\", name_1, name_2, ...)]"));
			}
			else if n_fields==0 && json_names.len()==1 && group_name.is_none()
			{	group_name = Some(json_names.pop().unwrap());
			}
			else
			{	return Err(format!("Must specify names for each member"));
			}
		}
	}
	Ok((group_name, json_names))
}

fn get_json_ignore(attrs: &Vec<Attribute>, n_variant: usize, json_ignore: &mut HashMap<String, HashSet<usize>>, is_ignore_all: bool) -> Result<bool, String>
{	let mut has_ignore = false;
	for a in attrs
	{	match a.parse_meta()
		{	Ok(Meta::Path(path)) =>
			{	if path.is_ident("json_ignore")
				{	has_ignore = true;
				}
			},
			Ok(Meta::List(list)) =>
			{	if list.path.is_ident("json_ignore")
				{	has_ignore = true;
					for a in list.nested
					{	let name = match a
						{	NestedMeta::Lit(Lit::Str(s)) =>
							{	if is_ignore_all
								{	return Err("#[json_ignore] after ignoring all".to_string());
								}
								Some(s.value())
							},
							NestedMeta::Meta(Meta::Path(meta)) =>
							{	if let Some(name) = meta.get_ident()
								{	if is_ignore_all
									{	return Err("#[json_ignore] after ignoring all".to_string());
									}
									Some(name.to_string())
								}
								else
								{	None
								}
							},
							_ =>
							{	None
							}
						};
						if let Some(name) = name
						{	match json_ignore.get_mut(&name)
							{	Some(variants) =>
								{	variants.insert(n_variant);
								}
								None =>
								{	let mut variants = HashSet::new();
									variants.insert(n_variant);
									json_ignore.insert(name, variants);
								}
							}
						}
					}
				}
			},
			_ => {}
		}
	}
	Ok(is_ignore_all || has_ignore && json_ignore.is_empty())
}

/// get generic parameters of this type (like struct<T> {...})
fn get_generics_debug_to_json(generics: &Generics) -> (ImplGenerics, TypeGenerics, proc_macro2::TokenStream)
{	let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
	// for each generic type add where: DebugToJson
	let mut wher = quote!();
	let mut i = 0;
	for p in &generics.params
	{	match p
		{	GenericParam::Type(ty) =>
			{	let ty = &ty.ident;
				if i == 0
				{	wher = if where_clause.is_none() {quote!(where)} else {quote!(#where_clause,)};
					wher = quote!( #wher #ty: nop_json::DebugToJson );
				}
				else
				{	wher = quote!( #wher, #ty: nop_json::DebugToJson );
				}
				i += 1;
			},
			_=> {},
		}
	}
	if i == 0
	{	wher = quote!(#where_clause);
	}
	(impl_generics, ty_generics, wher)
}

/// get generic parameters of this type (like struct<T> {...})
fn get_generics_write_to_json(generics: &mut Generics) -> (ImplGenerics, proc_macro2::TokenStream, proc_macro2::TokenStream)
{	let (_impl_generics, ty_generics, where_clause) = generics.split_for_impl();
	// for each generic type add where: WriteToJson
	let mut wher = if where_clause.is_none() {quote!(where WriteToJsonPriv1: std::io::Write)} else {quote!(#where_clause, WriteToJsonPriv1: std::io::Write)};
	for p in &generics.params
	{	match p
		{	GenericParam::Type(ty) =>
			{	let ty = &ty.ident;
				wher = quote!( #wher, #ty: nop_json::WriteToJson<WriteToJsonPriv1> );
			},
			_=> {},
		}
	}
	// add WriteToJsonPriv1 to impl_generics, but not to ty_generics
	let ty_generics = quote!(#ty_generics);
	let ident = Ident::new("WriteToJsonPriv1", Span::call_site());
	generics.params.push(GenericParam::Type(TypeParam {attrs: Default::default(), ident, colon_token: None, bounds: Default::default(), eq_token: None, default: None}));
	let impl_generics = generics.split_for_impl().0;
	(impl_generics, ty_generics, wher)
}


/// To generate WriteToJson implementation for any struct or enum, where all members also implement WriteToJson
/// use `#[derive(WriteToJson)]`.
///
/// See nop_json crate for details.
#[proc_macro_derive(WriteToJson, attributes(json))]
pub fn derive_write_to_json(input: TokenStream) -> TokenStream
{	let ast: &mut DeriveInput = &mut syn::parse(input).unwrap();
	match impl_debug_or_write_to_json(ast, true)
	{	Ok(ts) => ts,
		Err(error) =>
		{	panic!(error);
		}
	}
}

/// To generate ValidateJson implementation that always passes the validation use `#[derive(ValidateJson)]`.
///
/// See nop_json crate for details.
#[proc_macro_derive(ValidateJson)]
pub fn derive_validate_json(input: TokenStream) -> TokenStream
{	let ast: &mut DeriveInput = &mut syn::parse(input).unwrap();
	match impl_validate_json(ast)
	{	Ok(ts) => ts,
		Err(error) =>
		{	panic!(error);
		}
	}
}

fn impl_validate_json(ast: &mut DeriveInput) -> Result<TokenStream, String>
{	let name = &ast.ident; // struct or enum name
	// get generic parameters of this type (like struct<T> {...})
	let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
	// impl WriteToJson
	let code = quote!
	{	impl #impl_generics nop_json::ValidateJson for #name #ty_generics #where_clause {}
	};
	// to see what i produced, uncomment the panic!() below, and try to compile your code with #[derive(ValidateJson)]
//panic!(code.to_string());
	// done
	Ok(code.into())
}
