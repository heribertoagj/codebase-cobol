      *****************************************************************
      * NOME DA INC - I#GFCT51                                        *
      * DESCRICAO   - ARQUIVO REDUZIDO PARA PESQUISA SIGB             *
      * TAMANHO     - 0050                                            *
      * DATA        - 09.2007                                         *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 30689902 R.227 *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCT51-REDUZ23A.
           03 GFCT51-AGENCIA                 PIC  9(005)      COMP-3.
           03 GFCT51-CONTA                   PIC  9(007)      COMP-3.
           03 GFCT51-GRUPO                   PIC  9(005)      COMP-3.
           03 GFCT51-CARTEIRA                PIC  9(005)      COMP-3.
           03 GFCT51-DADO-CONS               PIC S9(015)      COMP-3.
           03 GFCT51-DADO-CONS-R  REDEFINES  GFCT51-DADO-CONS
                                             PIC  X(008).
           03 GFCT51-CPF-CNPJ-INT.
              05 GFCT51-CPF-CNPJ             PIC  S9(009)     COMP-3.
              05 GFCT51-FILIAL               PIC  S9(005)     COMP-3.
              05 GFCT51-CTRL                 PIC  9(002).
           03 GFCT51-TIPO-CTA                PIC  9(002).
           03 GFCT51-TIPO-PSSOA              PIC  X(001).
           03 FILLER                         PIC  X(016).
ST2507     03 GFCT51-CPF-CNPJ                PIC  X(009).
ST2507     03 GFCT51-FILIAL                  PIC  X(004).
ST2507     03 GFCT51-CTRL                    PIC  9(002).
