      ******************************************************************
      * NOME DA INC - GFCTWAKZ                                         *
      * DESCRICAO   - COMUNICACAO WEB X IMS                 - ENTRADA  *
      *               DEFER. FLEXIBIL. P/ AG/CTA                       *
      *               COMBO CONJUNTO SERVICOS              GFCT5211    *
      * TAMANHO     - 200                                              *
      * DATA        - 23.01.2010                                       *
      * RESPONSAVEL - CIBELE             - SONDAPROCWORK               *
      ******************************************************************
      *
       01  WAKZ-ENTRADA.
           05  WAKZ-LL                      PIC S9(004) COMP.
           05  WAKZ-ZZ                      PIC S9(004) COMP.
           05  WAKZ-TRANSACAO               PIC  X(009).
           05  WAKZ-FUNCAO                  PIC  X(008).
           05  WAKZ-DADOS-RST.
               10  WAKZ-QTDE-OCOR           PIC  9(003).
               10  WAKZ-QTDE-TOT-REG        PIC  9(006).
               10  WAKZ-FUNC-BDSCO          PIC  X(007).
               10  WAKZ-FILTRO.
                   15  WAKZ-FLT-AGENCIA     PIC  9(005).
                   15  WAKZ-FLT-CONTA       PIC  9(013).
               10  WAKZ-PONTEIRO.
                   15  WAKZ-PTR-QTDE-REGS   PIC  9(005).
           05  WAKZ-FIM                     PIC  X(001).
           05  FILLER                       PIC  X(139).
