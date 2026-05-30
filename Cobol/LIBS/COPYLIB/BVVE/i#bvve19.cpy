      *****************************************************************
      *   I#BVVE19 - AREA DE COMUNICACAO  DO ISD                      *
      *              ENVIO   PLATAFORMA BAIXA    X  MAINFRAME         *
      *-----         ENVIO DO ISD PARA M.F (BVVE7910)                 *
      *---------------------------------------------------------------*
      *   TAMANHO =    2048 BYTES                                     *
      *                                                               *
      *****************************************************************
      *
       01  BVVE19-AREA-ENTRADA.
           05 BVVE19-ENT-LL               PIC 9(04) COMP.
           05 BVVE19-ENT-ZZ               PIC 9(04) COMP.
           05 BVVE19-ENT-TRANSACAO        PIC X(09).
           05 BVVE19-NM-MODULO            PIC X(08).
           05 BVVE19-AREA-ENTRADA-DADOS   PIC X(2027) VALUE SPACES.
