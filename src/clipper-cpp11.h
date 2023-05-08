#pragma once

#include <iostream> // std::cout

#include "clipper.h"
#include "cpp11.hpp"
#include <cpp11/protect.hpp>

using namespace cpp11;

[[cpp11::register]]
writable::integers cpp_pip(
        const list layer,
        const list xy);
