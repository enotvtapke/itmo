package ru.itmo.wp.service;

import org.springframework.stereotype.Service;
import ru.itmo.wp.domain.Comment;
import ru.itmo.wp.domain.Post;
import ru.itmo.wp.domain.Role;
import ru.itmo.wp.domain.Tag;
import ru.itmo.wp.repository.PostRepository;
import ru.itmo.wp.repository.TagRepository;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Service
public class PostService {
    private final PostRepository postRepository;

    /** @noinspection FieldCanBeLocal, unused */
    private final TagRepository tagRepository;

    public PostService(PostRepository postRepository, TagRepository tagRepository) {
        this.postRepository = postRepository;
        this.tagRepository = tagRepository;
//        for (Tag.Name name : Tag.Name.values()) {
//            if (tagRepository.countByName(name) == 0) {
//                tagRepository.save(new Tag(name));
//            }
//        }
    }

    public List<Post> findAll() {
        return postRepository.findAllByOrderByCreationTimeDesc();
    }

    public Post findById(Long id) {
        return id == null ? null : postRepository.findById(id).orElse(null);
    }

    public void writeComment(Post post, Comment comment) {
        post.addComment(comment);
        postRepository.save(post);
    }

    public Tag findTag(String name) {
        return tagRepository.findByName(name);
    }

    public Set<Tag> getTags(String[] rawTags) {
        Set<Tag> tags = new HashSet<>();
        for (String tagName : rawTags) {
            if (tagName.equals("")) {
                continue;
            }
            Tag tag = findTag(tagName);
            if (tag == null) {
                tag = new Tag();
                tag.setName(tagName);
                tagRepository.save(tag);
            }
            tag = findTag(tagName);
            tags.add(tag);
        }
        return tags;
    }
}
