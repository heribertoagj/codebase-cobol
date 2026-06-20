      *****************************************************************
      * NOME DA INC - GFCTWAI6                                        *
      * DESCRICAO   - ARQUIVO CLIEADES                                *
      * DATA        - 02/09/2009                                      *
      * RESPONSAVEL - LAYS MOLEIRO - SONDA PROCWORK - 40 POS.         *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTWAI6-ENTRADA.
           03 WAI6-AGENCIA                PIC S9(005)    COMP-3.
           03 WAI6-RAZAO                  PIC S9(005)    COMP-3.
           03 WAI6-CONTA                  PIC  9(007)    COMP-3.
ST2507*    03 WAI6-CGC-CPF                PIC  9(009)    COMP-3.
ST2507*    03 WAI6-FILIAL                 PIC  9(005)    COMP-3.
ST2507     03 WAI6-CGC-CPF                PIC  X(009).
ST2507     03 WAI6-FILIAL                 PIC  X(004).
           03 WAI6-CONTR                  PIC  9(002).
           03 WAI6-POSTO-SERV             PIC  9(005)    COMP-3.
           03 WAI6-DT-ABERT               PIC  9(009)    COMP-3.
           03 FILLER                      PIC  X(012).
