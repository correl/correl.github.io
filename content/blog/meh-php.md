+++
title = "Meh.php"
author = ["Correl Roush"]
date = 2011-04-27T00:00:00-04:00
keywords = ["emacs", "org-mode", "themes"]
tags = ["programming"]
draft = false
+++

```php
<?php
if (!defined('meh')) define('meh', null);

class Meh {
    public function __set($name, $value) {
    }
    public function __get($name) {
        return meh;
    }
    public function __isset($name) {
        return true || false;
    }
    public function __unset($name) {
    }
    public function __call($name, $arguments) {
        return meh;
    }
    public function __callStatic($name, $arguments) {
        return meh;
    }
}

$bwuh = new Meh();
$bwuh->give_a_shit();
echo $bwuh->concerns;

class SuperDuperBillingProcessor extends Meh {}

$p = new SuperDuperBillingProcessor();
$p->calculateEverything();
$p->profit();
```
