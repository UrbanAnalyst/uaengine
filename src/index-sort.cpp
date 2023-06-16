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
    const size_t n_s = static_cast <size_t> (n);

    // Get number of bins, which requires copying std::vector to call
    // max_element:
    std::vector <int> index_in (n_s);
    std::copy (index0.begin (), index0.end (), index_in.begin ());
    const int n_bins_int = *std::max_element (index_in.begin (), index_in.end ());
    const size_t n_bins = static_cast <size_t> (n_bins_int);

    writable::integers res (n);
    std::fill (res.begin (), res.end (), -1L);

    std::vector <std::vector <int> > index_array (n_bins);
    for (size_t i = 0; i < n_bins; i++)
    {
        index_array [i] = std::vector <int> (0L);
    }

    // Then fill the 'n_bins' items of 'index_array', each of which has the
    // indices into the original values of `index0`.

    for (R_xlen_t i = 0; i < n; i++)
    {
        check_user_interrupt ();

        const size_t index_i = static_cast <size_t> (index0 [i] - 1L);

        std::vector <int> vec_i = index_array [index_i];

        vec_i.push_back (static_cast <int> (i));
        index_array [index_i] = vec_i;
    }

    // Use that to successfully fill the final index by successively looping
    // over the values of 'index_array'. The final index then hold index values
    // back into the original index.
    int pos = 0;
    while (pos < n)
    {
        check_user_interrupt ();
        for (size_t i = 0; i < n_bins; i++)
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
