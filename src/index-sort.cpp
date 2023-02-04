#include "iostream"
#include "vector"
#include "algorithm" // min
#include "iterator" // begin, end
#include "cpp11.hpp"
#include <unordered_set>

using namespace cpp11;

// Rearrange an index so that it sequentially samples equal groups of its
// values. The input is a very long index of some fewer number of values. This
// function rearranges the original index so that the values are sampled evenly.
// Note that this function returns a C++ 0-based index which must be converted
// to 1-based for use in R.

[[cpp11::register]]
writable::integers cpp_index_sort(integers index0)
{
    const R_xlen_t n = index0.size ();

    // Get number of bins, which requires copying std::vector to call
    // max_element:
    std::vector <int> index_in (n);
    std::copy (index0.begin (), index0.end (), index_in.begin ());
    const int n_bins = *std::max_element (index_in.begin (), index_in.end ());

    writable::integers res (n);
    std::fill (res.begin (), res.end (), -1L);

    std::vector <std::vector <int> > index_array (n_bins);
    for (int i = 0; i < n_bins; i++)
    {
        index_array [i] = std::vector <int> (0L);
    }

    for (R_xlen_t i = 0; i < n; i++)
    {
        check_user_interrupt ();

        std::vector <int> vec_i = index_array [index0 [i] - 1];

        vec_i.push_back (i);
        index_array [index0 [i] - 1] = vec_i;
    }

    // index_array then has `n_bins` items, each of which has the indices into
    // the original values of `index0`.

    int pos = 0;
    while (pos < n)
    {
        check_user_interrupt ();
        for (R_xlen_t i = 0; i < n_bins; i++)
        {
            std::vector <int> vec_i = index_array [i];
            if (vec_i.size () == 0L)
            {
                continue;
            }
            res [pos++] = vec_i.front ();
            vec_i.erase (vec_i.begin ());
            index_array [i] = vec_i;
        }
    }

    return res;
}
