package DoReport;

our %default_msg = (
  To => [ 'Norton Allen <allen@huarp.harvard.edu>',
          'Orfeo Colebatch <orfeo.colebatch@utoronto.ca>',
          'Kim Strong <strong@atmosp.physics.utoronto.ca>',
          'Erin McGee <e.mcgee@mail.utoronto.ca>',
          'jblavier <jblavier@caesar.jpl.nasa.gov>'
        ],
  From => 'moacc_ifs12 <tccon.cambridgebay@huarp.harvard.edu>',
  Subject => 'moacc_ifs12',
  Message => 'stuff',
  Message_ID_Template => 'moacc-ifs13-%d-%d-%d@huarp.harvard.edu'
);

1;
