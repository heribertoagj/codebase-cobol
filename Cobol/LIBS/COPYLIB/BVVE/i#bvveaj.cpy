      ***I#BVVEAJ*******************************************************
      *                                                                *
      *    +-----------------------------------------------------+     *
      *    |   INC DA ROTINA BVVE PARA ATENDER A INTERFACE DE    |     *
      *    |   SAIDA DO REPOSITORIO SAP                          |     *
      *    |                                                     |     *
      *    | OBS : INCLUINDO NOVOS CAMPOS ALTERAR O JOB:         |     *
      *    |       SAPI7999                                      |     *
      *    |                                                     |     *
      *    +-----------------------------------------------------+     *
      *                                         LRECL=0586             *
      ******************************************************************
       01  WK-BVVE0510.
           05 FILLER                     PIC X(013).
           05 WK-CJUNC-DEPDC-CREDT       PIC 9(005) COMP-3.
           05 WK-CCTA-CORR               PIC 9(007) COMP-3.
           05 FILLER                     PIC X(479).
           05 WK-CSIT-FUNC-BDSCO         PIC X(001).
           05 FILLER                     PIC X(086).

