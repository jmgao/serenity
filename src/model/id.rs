//! A collection of newtypes defining type-strong IDs.

use chrono::{FixedOffset, DateTime, NaiveDateTime};
use crate::internal::prelude::*;
use serde::de::{Deserialize, Deserializer};
use std::fmt::{Display, Formatter, Result as FmtResult};
use super::utils::U64Visitor;

macro_rules! id_u64 {
    ($($name:ident;)*) => {
        $(
            impl $name {
                /// Retrieves the time that the Id was created at.
                pub fn created_at(&self) -> DateTime<FixedOffset> {
                    let offset = self.0 >> 22;
                    let secs = offset / 1000;
                    let millis = (offset % 1000) * 1_000_000; // 1 million nanoseconds in a millisecond

                    let tm = NaiveDateTime::from_timestamp(1_420_070_400 + secs as i64, millis as u32);
                    DateTime::from_utc(tm, FixedOffset::east(0))
                }

                /// Immutably borrow inner Id.
                #[inline]
                pub fn as_u64(&self) -> &u64 {
                    &self.0
                }

                /// Mutably borrow inner Id.
                #[inline]
                pub fn as_mut_u64(&mut self) -> &mut u64 {
                    &mut self.0
                }
            }

            // This is a hack so functions can accept iterators that either:
            // 1. return the id itself (e.g: `MessageId`)
            // 2. return a reference to it (`&MessageId`).
            impl AsRef<$name> for $name {
                fn as_ref(&self) -> &Self {
                    self
                }
            }

            impl<'a> From<&'a $name> for $name {
                fn from(id: &'a $name) -> $name {
                    id.clone()
                }
            }

            impl From<u64> for $name {
                fn from(id_as_u64: u64) -> $name {
                    $name(id_as_u64)
                }
            }

            impl PartialEq<u64> for $name {
                fn eq(&self, u: &u64) -> bool {
                    self.0 == *u
                }
            }

            impl Display for $name {
                fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
                    Display::fmt(&self.0, f)
                }
            }

            impl<'de> Deserialize<'de> for $name {
                fn deserialize<D: Deserializer<'de>>(deserializer: D) -> StdResult<Self, D::Error> {
                    deserializer.deserialize_any(U64Visitor).map($name)
                }
            }

            impl From<$name> for u64 {
                fn from(id: $name) -> u64 {
                    id.0 as u64
                }
            }

            impl From<$name> for i64 {
                fn from(id: $name) -> i64 {
                    id.0 as i64
                }
            }

            #[cfg(feature = "diesel")]
            impl<DB> diesel::serialize::ToSql<diesel::sql_types::Integer, DB> for $name
            where
                DB: diesel::backend::Backend,
                i64: diesel::serialize::ToSql<diesel::sql_types::Integer, DB>,
            {
                fn to_sql<W: std::io::Write>(&self, out: &mut diesel::serialize::Output<'_, W, DB>) -> diesel::serialize::Result {
                    (self.0 as i64).to_sql(out)
                }
            }

            #[cfg(feature = "diesel")]
            impl<DB> diesel::deserialize::FromSql<diesel::sql_types::Integer, DB> for $name
            where
                DB: diesel::backend::Backend,
                i64: diesel::deserialize::FromSql<diesel::sql_types::Integer, DB>,
            {
                fn from_sql(bytes: Option<&DB::RawValue>) -> diesel::deserialize::Result<Self> {
                    Ok($name(i64::from_sql(bytes)? as u64))
                }
            }

            #[cfg(feature = "diesel")]
            impl<DB> diesel::serialize::ToSql<diesel::sql_types::BigInt, DB> for $name
            where
                DB: diesel::backend::Backend,
                i64: diesel::serialize::ToSql<diesel::sql_types::BigInt, DB>,
            {
                fn to_sql<W: std::io::Write>(&self, out: &mut diesel::serialize::Output<'_, W, DB>) -> diesel::serialize::Result {
                    (self.0 as i64).to_sql(out)
                }
            }

            #[cfg(feature = "diesel")]
            impl<DB> diesel::deserialize::FromSql<diesel::sql_types::BigInt, DB> for $name
            where
                DB: diesel::backend::Backend,
                i64: diesel::deserialize::FromSql<diesel::sql_types::BigInt, DB>,
            {
                fn from_sql(bytes: Option<&DB::RawValue>) -> diesel::deserialize::Result<Self> {
                    Ok($name(i64::from_sql(bytes)? as u64))
                }
            }

            #[cfg(feature = "diesel")]
            impl<ST, DB> diesel::query_source::Queryable<ST, DB> for $name
            where
                i64: diesel::types::FromSqlRow<ST, DB>,
                DB: diesel::backend::Backend,
                DB: diesel::types::HasSqlType<ST>,
            {
                type Row = i64;

                fn build(row: Self::Row) -> Self {
                    $name(row as u64)
                }
            }

        )*
    }
}

#[cfg(feature = "diesel")]
use diesel::AsExpression;

/// An identifier for an Application.
#[derive(Copy, Clone, Default, Debug, Eq, Hash, PartialEq, PartialOrd, Ord, Serialize)]
#[cfg_attr(feature = "diesel", derive(AsExpression))]
#[cfg_attr(feature = "diesel", sql_type = "diesel::sql_types::BigInt")]
pub struct ApplicationId(pub u64);

/// An identifier for a Channel
#[derive(Copy, Clone, Default, Debug, Eq, Hash, PartialEq, PartialOrd, Ord, Serialize)]
#[cfg_attr(feature = "diesel", derive(AsExpression))]
#[cfg_attr(feature = "diesel", sql_type = "diesel::sql_types::BigInt")]
pub struct ChannelId(pub u64);

/// An identifier for an Emoji
#[derive(Copy, Clone, Default, Debug, Eq, Hash, PartialEq, PartialOrd, Ord, Serialize)]
#[cfg_attr(feature = "diesel", derive(AsExpression))]
#[cfg_attr(feature = "diesel", sql_type = "diesel::sql_types::BigInt")]
pub struct EmojiId(pub u64);

/// An identifier for a Guild
#[derive(Copy, Clone, Default, Debug, Eq, Hash, PartialEq, PartialOrd, Ord, Serialize)]
#[cfg_attr(feature = "diesel", derive(AsExpression))]
#[cfg_attr(feature = "diesel", sql_type = "diesel::sql_types::BigInt")]
pub struct GuildId(pub u64);

/// An identifier for an Integration
#[derive(Copy, Clone, Default, Debug, Eq, Hash, PartialEq, PartialOrd, Ord, Serialize)]
#[cfg_attr(feature = "diesel", derive(AsExpression))]
#[cfg_attr(feature = "diesel", sql_type = "diesel::sql_types::BigInt")]
pub struct IntegrationId(pub u64);

/// An identifier for a Message
#[derive(Copy, Clone, Default, Debug, Eq, Hash, PartialEq, PartialOrd, Ord, Serialize)]
#[cfg_attr(feature = "diesel", derive(AsExpression))]
#[cfg_attr(feature = "diesel", sql_type = "diesel::sql_types::BigInt")]
pub struct MessageId(pub u64);

/// An identifier for a Role
#[derive(Copy, Clone, Default, Debug, Eq, Hash, PartialEq, PartialOrd, Ord, Serialize)]
#[cfg_attr(feature = "diesel", derive(AsExpression))]
#[cfg_attr(feature = "diesel", sql_type = "diesel::sql_types::BigInt")]
pub struct RoleId(pub u64);

/// An identifier for a User
#[derive(Copy, Clone, Default, Debug, Eq, Hash, PartialEq, PartialOrd, Ord, Serialize)]
#[cfg_attr(feature = "diesel", derive(AsExpression))]
#[cfg_attr(feature = "diesel", sql_type = "diesel::sql_types::BigInt")]
pub struct UserId(pub u64);

/// An identifier for a [`Webhook`](../webhook/struct.Webhook.html).
#[derive(Copy, Clone, Default, Debug, Eq, Hash, PartialEq, PartialOrd, Ord, Serialize)]
#[cfg_attr(feature = "diesel", derive(AsExpression))]
#[cfg_attr(feature = "diesel", sql_type = "diesel::sql_types::BigInt")]
pub struct WebhookId(pub u64);

/// An identifier for an audit log entry.
#[derive(Copy, Clone, Default, Debug, Eq, Hash, PartialEq, PartialOrd, Ord, Serialize)]
#[cfg_attr(feature = "diesel", derive(AsExpression))]
#[cfg_attr(feature = "diesel", sql_type = "diesel::sql_types::BigInt")]
pub struct AuditLogEntryId(pub u64);

/// An identifier for an attachment.
#[derive(Copy, Clone, Default, Debug, Eq, Hash, PartialEq, PartialOrd, Ord, Serialize)]
#[cfg_attr(feature = "diesel", derive(AsExpression))]
#[cfg_attr(feature = "diesel", sql_type = "diesel::sql_types::BigInt")]
pub struct AttachmentId(u64);

id_u64! {
    AttachmentId;
    ApplicationId;
    ChannelId;
    EmojiId;
    GuildId;
    IntegrationId;
    MessageId;
    RoleId;
    UserId;
    WebhookId;
    AuditLogEntryId;
}
