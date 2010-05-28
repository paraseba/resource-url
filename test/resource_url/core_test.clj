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

