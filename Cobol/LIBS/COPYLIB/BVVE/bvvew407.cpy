      *---------------------------------------------------------------*
      *               H S T   C A R D   T E C H N O L O G Y           *
      *---------------------------------------------------------------*
      *   COPY BOOK   - BVVEW407                                      *
      *   DATA        - 21/06/2017                                    *
      *   ANALISTA    - HST                                           *
      *   PROGRAMADOR - HST                                           *
      *   OBJETIVO    - COPY BOOK DO PROCESSO DE INSERCAO LOG EVENTO  *
      *---------------------------------------------------------------*
      *         H I S T O R I C O   D E   M A N U T E N C O E S       *
      *-------------------------------------------------------------- *
      * DATA     | AUTOR     |    DESCRICAO                           *
      *----------+-----------+----------------------------------------*
      *21/06/2017| ERICA M.  | CODIFICACAO                            *
      *---------------------------------------------------------------*
       01  WRK-BVVE7407-COMM.
           05 WRK-BVVE7407-CONTROLE.
               10  WRK-BVVE7407-CD-MSG-RETORNO PIC  9(007) VALUE ZEROS.
               10  WRK-BVVE7407-MSG-RETORNO    PIC  X(080) VALUE SPACES.
           05  WRK-407-ENTRADA.
               10  WRK-BVVE7407-COD-LOG-EVENTO PIC  9(009) VALUE ZERO.
               10  WRK-BVVE7407-TIP-LOG-EVENTO PIC  X(003) VALUE SPACES.
               10  WRK-BVVE7407-AREA-EVENTO.
                   15 WRK-BVVE7407-AREA-EVENTO-LEN
                                              PIC S9(004) COMP VALUE +0.
                   15 WRK-BVVE7407-AREA-EVENTO-TXT
                                              PIC X(3500) VALUE SPACES.
