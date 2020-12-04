from functools import reduce


def get(collection, key):
    if isinstance(collection, dict):
        return collection.get(key)
    elif isinstance(collection, list):
        try:
            return collection[key]
        except (IndexError, TypeError):
            return None
    return None


def dig(collection, *keys):
    return reduce(get, keys, collection)


student_record = {
    "name": "Jimmy Jurner",
    "courses": [
        {"name": "biology", "grades": [48, 54, 68]},
        {"name": "physics", "grades": [40, 59, 69]},
        {"name": "chemistry", "grades": [60, 69, 75]},
        {"name": "computer science", "grades": []},
    ],
}

# None, because of TypeError - courses:collection["three"] when courses:collection is a list
print(dig(student_record, "courses", "three", "grades", 2))

# None, because of IndexError - grades:collection[2] when len(grades:collection) is 0
print(dig(student_record, "courses", 3, "grades", 2))

# None, because "teachers" is not a key in a course:collection
print(dig(student_record, "courses", 3, "teachers", 2))

# 68, because student_record["courses"][0]["grades"][2] is found and extracted
print(dig(student_record, "courses", 0, "grades", 2))
