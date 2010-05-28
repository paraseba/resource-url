(ns resource-url.core-test
  (:use [resource-url.core] :reload-all)
  (:use [clojure.test]))

(deftest urls-without-parameters

  (named-url news-path "news")
  (named-url best-news-path "news" "best")
  (named-url best-news-with-slash-path "news/" "best//")

  (is (= (news-path) "news"))
  (is (= (best-news-path) "news/best"))
  (is (= (best-news-with-slash-path) "news/best"))

  (testing "with anchor"
    (is (= (news-path :anchor 1) "news#1"))
    (is (= (best-news-path :anchor "this is my anchor") "news/best#this+is+my+anchor"))))

(deftest urls-with-parameters

  (named-url user-path "users" :id)
  (named-url user-images-path "users" :id "images")
  (named-url user-images-with-slashes-path "users//" :id "images///")
  (named-url user-image-path "users" :id "images" #(:title %))

  (testing "no extra parameters"
    (is (= (user-path {:id 1}) "users/1"))
    (is (= (user-images-path {:id "foo"}) "users/foo/images"))
    (is (= (user-images-with-slashes-path {:id :foo}) "users/foo/images"))
    (is (= (user-image-path {:id :foo} {:title :bar}) "users/foo/images/bar"))
    (is (= (user-image-path {:id :foo} {:title "bar//"}) "users/foo/images/bar")))

  (testing "with extra parameters"
    (is (= (user-path {:id :foo} "sort" "recent" :value 1) "users/foo/?value=1&sort=recent"))
    (is (= (user-image-path {:id :foo} {:title :bar} :value 1) "users/foo/images/bar/?value=1"))
    (is (= (user-path {:id :foo} "sort" "the recent" :value 1) "users/foo/?value=1&sort=the+recent")))

  (testing "with anchor"
    (is (= (user-path {:id :foo} :anchor :my-id "sort" "the recent" :value 1) "users/foo/?value=1&sort=the+recent#my-id"))))



(polymorphic-url url-for-type by-type)
(polymorphic-url url-for-key (by-member :dispatch))

(defrecord Post [id url-slug dispatch])
(defrecord Comment [id dispatch])

(named-url post-path "posts" :url-slug)
(named-url comment-path "posts" :url-slug "comments" :id)

(resource-url url-for-type Post post-path)
(resource-url url-for-type Comment comment-path)

(resource-url url-for-key 1 post-path)
(resource-url url-for-key 2 comment-path)


(deftest resources-urls
  (let [p (Post. 1 "the-post" 1)
        c (Comment. 2 2)]
    (is (= (url-for-type p) "posts/the-post"))
    (is (= (url-for-key  p) "posts/the-post"))
    (is (= (url-for-type [p c]) "posts/the-post/comments/2"))
    (is (= (url-for-key  [p c]) "posts/the-post/comments/2"))
    (is (= (url-for-type [p c] :key :value :anchor :the-anchor) "posts/the-post/comments/2/?key=value#the-anchor"))
    (is (= (url-for-key  [p c] :key :value :anchor :the-anchor) "posts/the-post/comments/2/?key=value#the-anchor"))))

