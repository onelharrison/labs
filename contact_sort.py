"""
The following code explores the presentation of contacts
in an on-screen phone book in which a contact's
first name xor last name may be missing and contacts can
be sorted by first name xor last name.

A true sort by first name and last name is implemented as well as
correspending sort procedures with a presentation bias. In effect,
the presentation bias makes the contacts in the on-screen phone book
appear to be sorted by their intended sort key but in really this may
not be true. However, because of how the contacts are displayed, there
is no visual consequence in the case of contacts that are missing a
first name xor last name.

A presentation-biased sort may be preferable to a true sort as the
presentation-biased sort is visually within expectation while a true
sort may betray visual expectation. See the outputs of the sorts by last name.
"""

from collections import namedtuple

Contact = namedtuple("Contact", "fname lname")

contacts = [
    Contact("Lex", "Luther"),
    Contact("Firestorm", "Lobo"),
    Contact("Green", "Lantern"),
    Contact("Aquaman", None),
    Contact(None, "Diner"),
    Contact(None, "Metropolis"),
]


def coalesce(*values):
    """Returns the first not-None arguement or None"""
    return next((v for v in values if v is not None), None)


def display_contacts(contacts):
    """Displays contacts"""
    for contact in contacts:
        print(f"{(contact.fname or '')} {(contact.lname or '')}".strip())


def presentation_sort_by_first_name(contacts):
    """Returns a sorted sequence of contacts with a presentation bias.

    Tries to sort by first name, but if the first name isn't present, the last name is used instead
    """
    return sorted(contacts, key=lambda c: coalesce(c.fname, c.lname))


def presentation_sort_by_last_name(contacts):
    """Returns a sorted sequence of contacts with a presentation bias.

    Tries to sort by last name, but if the last name isn't present, the first name is used instead
    """
    return sorted(contacts, key=lambda c: coalesce(c.lname, c.fname))


def sort_by_first_name(contacts):
    """Returns a sorted sequence of contacts with a presentation bias.

    Truly sorts by first name
    """
    return sorted(contacts, key=lambda c: coalesce(c.fname, ""))


def sort_by_last_name(contacts):
    """Returns a sorted sequence of contacts with a presentation bias.

    Truly sorts by last name
    """
    return sorted(contacts, key=lambda c: coalesce(c.lname, ""))


print("True Sort by First Name")
display_contacts(sort_by_first_name(contacts))
print()

print("Sort by First Name (with presentation bias)")
display_contacts(presentation_sort_by_first_name(contacts))
print()

print("True Sort by Last Name")
display_contacts(sort_by_last_name(contacts))
print()

print("Sort by Last Name (with presentation bias)")
display_contacts(presentation_sort_by_last_name(contacts))
print()
