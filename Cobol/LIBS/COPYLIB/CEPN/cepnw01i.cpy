      *----------------------------------------------------------------*
      *    INC CEPNW01I         - INTERFACE DO COORDENADOR E FUNCIONAL *
      *                                                                *
      *    TAMANHO              -  7369 BYTES                          *
      *                                                                *
      *    ANALISTA RESPONSAVEL - MAYARA PINHEIRO                      *
      *    ATUALIZACAO          - 24/05/2010                           *
      *                                                                *
      *    DESCRICAO            - INTERFACE DO COORDENADOR E FUNCIONAL *
      *----------------------------------------------------------------*
      *
           05  CEPNW01I-HEADER.
               10 CEPNW01I-COD-LAYOUT   PIC X(008) VALUE 'CEPNW01I'.
               10 CEPNW01I-TAM-LAYOUT   PIC 9(005) VALUE 07369.
           05 CEPNW01I-REGISTRO.
               10  CEPNW01I-DADOS-ENTRADA.
                   15  CEPNW01I-E-MAX-OCORR        PIC 9(005).
                   15  CEPNW01I-E-NUM-CEP          PIC 9(005).
                   15  CEPNW01I-E-COMPL-CEP        PIC X(003).
               10  CEPNW01I-BLOCO-PAGINACAO.
                   15 CEPNW01I-INDICADOR-PAGINACAO PIC X(001).
                      88 CEPNW01I-P-INICIAL        VALUE 'I'.
                      88 CEPNW01I-P-PRIMEIRA       VALUE 'P'.
                      88 CEPNW01I-P-SEGUINTE       VALUE 'S'.
                      88 CEPNW01I-P-ANTERIOR       VALUE 'A'.
                      88 CEPNW01I-P-ULTIMA         VALUE 'U'.
                   15 CEPNW01I-CHAVE-INI.
                      20 CEPNW01I-INI-NUM-CEP      PIC 9(005).
                      20 CEPNW01I-INI-COMPL-CEP    PIC 9(003).
                      20 CEPNW01I-INI-DES-CEP      PIC X(060).
                   15 CEPNW01I-CHAVE-FIM.
                      20 CEPNW01I-FIM-NUM-CEP      PIC 9(005).
                      20 CEPNW01I-FIM-COMPL-CEP    PIC 9(003).
                      20 CEPNW01I-FIM-DES-CEP      PIC X(060).
               10  CEPNW01I-S-BLOCO-SAIDA.
                   15 CEPNW01I-S-OCOR                  PIC 9(005).
                   15 CEPNW01I-S-DADOS-N-ENCONTRADOS   PIC  X(01).
                   15 CEPNW01I-REG-SAIDA   OCCURS 0 TO 50 TIMES
                      DEPENDING ON         CEPNW01I-S-OCOR.
                      20  CEPNW01I-S-NUME-CEPX             PIC  9(005).
                      20  CEPNW01I-S-COMPLE-CEP            PIC  9(003).
                      20  CEPNW01I-S-ENDER                 PIC  X(060).
                      20  CEPNW01I-S-BAIRRO                PIC  X(020).
                      20  CEPNW01I-S-CIDADEX               PIC  X(030).
                      20  CEPNW01I-S-UF                    PIC  X(002).
                      20  CEPNW01I-S-COD-ENTRX             PIC  X(001).
                      20  CEPNW01I-S-COD-MUN               PIC  9(005)V.
                      20  CEPNW01I-S-DIG-CD-MUN            PIC  X(001).
                      20  CEPNW01I-S-AGE-DEP               PIC  9(007)V.
                      20  CEPNW01I-S-TPO-ENDER             PIC  X(005).
                      20  CEPNW01I-S-COD-DISTR             PIC  9(005)V.

