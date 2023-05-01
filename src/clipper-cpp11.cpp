#include "clipper-cpp11.h"

// clipper only works with integers, so double values have to be multiplied by
// this amount before converting to int:
const long long mult = 1e6;

//' rcpp_pip
//'
//' polys List of polygons for which to extract membership
//' @noRd
writable::integers cpp_pip(
        const list polys,
        const list xy)
{
    const doubles x = xy ["x"];
    const doubles y = xy ["y"];
    const int n = x.size ();

    writable::integers res (n);
    std::fill (res.begin (), res.end (), -1L);
    for (int i = 0; i < polys.size (); i++)
    {
        doubles pi = polys [i];
        const int nrow = pi.size () / 2;
        
        ClipperLib::Path path;
        for (size_t j = 0; j < nrow; j++)
        {
            path << ClipperLib::IntPoint (round (pi [j] * mult),
                    round (pi [nrow + j] * mult));
        }

        for (size_t j = 0; j < n; j++)
        {
            const ClipperLib::IntPoint pj =
                ClipperLib::IntPoint (round (x [j] * mult),
                                      round (y [j] * mult));
            int pip = ClipperLib::PointInPolygon (pj, path);
            if (pip != 0) res [j] = i;
        }

        const double progress = 100 * i / polys.size ();
        std::cout << "\r" << i << " / " << polys.size () << ": " <<
            progress << "%";
        std::cout.flush ();
        check_user_interrupt ();
    }
    std::cout << "\r" << polys.size () << " / " << polys.size () <<
        ": 100%" << std::endl;

    return res;
}
