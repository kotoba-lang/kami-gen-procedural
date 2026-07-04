(ns kami.gen.procedural-test
  "End-to-end coverage of `compose-costumed-character`: asserts on the real
  SHAPE of the returned map (part/vertex/bone counts, a real parseable VRM
  round-trip via `vrm.parse`/`vrm.humanoid` — the same functions
  `kotoba-lang/vrm`'s own test suite uses to validate a compose+export
  round-trip), not just \"doesn't throw\"."
  (:require [clojure.test :refer [deftest is testing]]
            [kami.gen.procedural :as proc]
            [vrm.parse :as vrm-parse]
            [vrm.humanoid :as vrm-humanoid]
            [vrm.vrm-types :as vt]))

(deftest compose-costumed-character-shape-test
  (testing "top-level shape"
    (let [result (proc/compose-costumed-character {:base :race/human :costume :penguin-kigurumi :seed 42})]
      (is (= #{:body :costume :vrm :render/profile} (set (keys result))))
      (is (= #{:parts :skeleton :blendshape-targets} (set (keys (:body result)))))
      (is (seq (:parts (:body result))))
      (is (seq (:bones (:skeleton (:body result)))))))

  (testing "body: chibi proportions actually changed the generated skeleton height"
    (let [result (proc/compose-costumed-character {:seed 1 :proportions {:limb-scale 0.6}})
          bones (:bones (:skeleton (:body result)))
          hips (first (filter #(= (:name %) "hips") bones))
          left-lower-leg (first (filter #(= (:name %) "leftLowerLeg") bones))]
      ;; character.body/generate-humanoid-skeleton scales thigh/shin length
      ;; by `height` (here driven by :limb-scale) -- a chibi 0.6 limb-scale
      ;; must produce a visibly shorter thigh bone offset than the
      ;; unscaled 1.0 default.
      (is (some? hips))
      (is (some? left-lower-leg))
      (is (< (Math/abs (nth (:local-position left-lower-leg) 1)) 0.11))))

  (testing "body: \"body\" part carries real skin weights on every vertex"
    (let [result (proc/compose-costumed-character {:seed 42})
          body-part (some #(when (= (:name %) "body") %) (:parts (:body result)))]
      (is (some? body-part))
      (is (seq (:vertices body-part)))
      (is (every? :joint-indices (:vertices body-part)))
      (is (every? #(= 4 (count (:joint-indices %))) (:vertices body-part)))
      (is (every? #(< 0.99 (reduce + (:joint-weights %)) ) (:vertices body-part)))))

  (testing "costume: hood/beak mesh is real, non-empty, split into exactly hood+beak materials"
    (let [result (proc/compose-costumed-character {:seed 42})
          hood-parts (:hood (:costume result))
          materials (set (map :material hood-parts))]
      (is (= #{:hood :beak} materials))
      (doseq [part hood-parts]
        (is (pos? (count (:vertices part))))
        (is (pos? (count (:indices part))))
        (is (zero? (mod (count (:indices part)) 3)))
        (is (every? #(= 3 (count (:position %))) (:vertices part))))))

  (testing "costume: body-suit is a padded, still-skinned copy of the body mesh"
    (let [result (proc/compose-costumed-character {:seed 42})
          body-part (some #(when (= (:name %) "body") %) (:parts (:body result)))
          suit-part (:body-suit (:costume result))]
      (is (= (count (:vertices body-part)) (count (:vertices suit-part))))
      (is (every? :joint-indices (:vertices suit-part)))
      ;; every vertex actually moved outward (padding > 0, along a nonzero normal)
      (is (every? (fn [[a b]] (not= (:position a) (:position b)))
                  (map vector (:vertices body-part) (:vertices suit-part))))))

  (testing "vrm: real GLB magic + real VRM 1.0 round-trip"
    (let [result (proc/compose-costumed-character {:seed 7})
          glb-bytes (:glb-bytes (:vrm result))]
      (is (pos? (count glb-bytes)))
      ;; GLB magic "glTF" little-endian header, per glTF 2.0 binary container spec.
      (is (= [0x67 0x6C 0x54 0x46] (vec (take 4 glb-bytes))))
      (let [parsed (vrm-parse/parse-vrm glb-bytes)]
        (is (= :v1-0 (:version parsed)))
        (is (seq (:human-bones (:humanoid parsed))))
        ;; every VRM 1.0 required-ish core bone (hips/spine/chest/neck/head)
        ;; must resolve to a real node.
        (doseq [required [:hips :spine :chest :neck :head
                          :left-upper-arm :right-upper-arm
                          :left-upper-leg :right-upper-leg]]
          (is (some #(= (:bone %) required) (:human-bones (:humanoid parsed)))
              (str "missing required humanoid bone " required)))
        (testing "vrm.humanoid/to-kami-skeleton succeeds against our own exported skin"
          (let [skel (vrm-humanoid/to-kami-skeleton parsed)]
            (is (= (count (:bones (:skeleton (:body result)))) (count (:bones skel))))))
        (testing "meta name reflects base/costume/seed"
          (is (re-find #"kami-gen-procedural/race/human/penguin-kigurumi/7"
                       (:name (:meta parsed))))))))

  (testing "render/profile: kami-isekai-assets-style flat fallback"
    (let [result (proc/compose-costumed-character {:seed 42})
          profile (:render/profile result)]
      (is (= #{:color :w :h :geo :emissive} (set (keys profile))))
      (is (= 3 (count (:color profile))))
      (is (pos? (:w profile)))
      (is (pos? (:h profile)))))

  (testing "determinism: same seed -> identical palette; different seed -> different palette"
    (let [a (proc/compose-costumed-character {:seed 5})
          b (proc/compose-costumed-character {:seed 5})
          c (proc/compose-costumed-character {:seed 6})]
      (is (= (get-in a [:costume :params :palette]) (get-in b [:costume :params :palette])))
      (is (not= (get-in a [:costume :params :palette]) (get-in c [:costume :params :palette])))))

  (testing "every tunable is overridable via plain EDN, not hidden"
    (let [small-head (proc/compose-costumed-character {:seed 1 :proportions {:head-scale 1.0 :limb-scale 0.6}})
          big-head (proc/compose-costumed-character {:seed 1 :proportions {:head-scale 3.0 :limb-scale 0.6}})
          head-extent (fn [result]
                        (let [head (some #(when (= (:name %) "head") %) (:parts (:body result)))]
                          (apply max (map (fn [{[x _ _] :position}] (Math/abs x)) (:vertices head)))))
          result (proc/compose-costumed-character
                  {:seed 1
                   :kigurumi {:hood-radius 0.2 :beak-length 0.15
                              :palette {:hood-color [0.0 0.0 0.0] :beak-color [1.0 0.5 0.0]}}})]
      ;; :head-scale is a real post-hoc vertex scale, not a no-op knob.
      (is (< (head-extent small-head) (head-extent big-head)))
      (is (= 0.2 (get-in result [:costume :params :hood-radius])))
      (is (= 0.15 (get-in result [:costume :params :beak-length])))))

  (testing "unsupported base/costume throws rather than silently falling back"
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (proc/compose-costumed-character {:base :race/elf})))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (proc/compose-costumed-character {:costume :unknown-costume})))))

(deftest publish-to-asset-hub-test
  (let [result (proc/compose-costumed-character {:seed 42})
        entry (proc/publish-to-asset-hub result)]
    (is (= :model3d (:asset/kind entry)))
    (is (= :vrm (:asset/format entry)))
    (is (re-find #"^bafy-sha256-[0-9a-f]{64}$" (get-in entry [:asset/payload :cid :hash])))
    (is (pos? (get-in entry [:asset/payload :bytes])))
    (is (true? (get-in entry [:asset/preview :webgpu])))
    (is (contains? (set (:asset/deps entry)) "kotoba-lang/kami-gen-procedural"))))
