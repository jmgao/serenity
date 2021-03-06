use crate::internal::prelude::*;
use super::CreateEmbed;
use crate::utils;

use std::collections::HashMap;

/// A builder to specify the fields to edit in an existing message.
///
/// # Examples
///
/// Editing the content of a [`Message`] to `"hello"`:
///
/// ```rust,no_run
/// # use serenity::model::id::{ChannelId, MessageId};
/// # #[cfg(feature = "client")]
/// # use serenity::client::Context;
/// # #[cfg(feature = "framework")]
/// # use serenity::framework::standard::{CommandResult, macros::command};
/// #
/// # #[cfg(all(feature = "http", feature = "framework"))]
/// # #[command]
/// # fn example(ctx: &mut Context) -> CommandResult {
/// # let mut message = ChannelId(7).message(&ctx.http, MessageId(8)).unwrap();
/// let _ = message.edit(ctx, |m| {
///     m.content("hello")
/// });
/// # Ok(())
/// # }
/// #
/// # fn main() {}
/// ```
///
/// [`Message`]: ../model/channel/struct.Message.html
#[derive(Clone, Debug, Default)]
pub struct EditMessage(pub HashMap<&'static str, Value>);

impl EditMessage {
    /// Set the content of the message.
    ///
    /// **Note**: Message contents must be under 2000 unicode code points.
    #[inline]
    pub fn content<D: ToString>(&mut self, content: D) -> &mut Self {
        self.0.insert("content", Value::String(content.to_string()));
        self
    }

    /// Set an embed for the message.
    pub fn embed<F>(&mut self, f: F) -> &mut Self
    where F: FnOnce(&mut CreateEmbed) -> &mut CreateEmbed {
        let mut create_embed = CreateEmbed::default();
        f(&mut create_embed);
        let map = utils::hashmap_to_json_map(create_embed.0);
        let embed = Value::Object(map);

        self.0.insert("embed", embed);
        self
    }
}
