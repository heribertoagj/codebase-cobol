      ******************************************************************
      * NOME DA INC - GFCTWAJZ                                         *
      * DESCRICAO   - COMUNICACAO WEB X IMS                 - SAIDA    *
      *               FLEXIBILIZACAO PARA AGRUPAMENTO                  *
      *               CONSULTA HISTORICOS                              *
      *               COMBO UF HISTORICO FLEXIBILIZACAO     - GFCT5207 *
      * TAMANHO     - 1010 - TOTAL                                     *
      *               0470 - FIXO                                      *
      *               0540 - VARIAVEL (27 X 20)                        *
      * DATA        - 27.11.2009                                       *
      * RESPONSAVEL - FRANCISCO LOURENCO - SONDAPROCWORK               *
      ******************************************************************
      *
       01  WAJZ-SAIDA.
           05  WAJZ-LL                 PIC S9(004) COMP.
           05  WAJZ-ZZ                 PIC S9(004) COMP.
           05  WAJZ-TRANSACAO          PIC  X(009).
           05  WAJZ-FUNCAO             PIC  X(008).
           05  WAJZ-DADOS-RST.
               10  WAJZ-QTDE-OCOR      PIC  9(007).
               10  WAJZ-QTDE-TOT-REG   PIC  9(009).
               10  WAJZ-FUNC-BDSCO     PIC  X(007).
               10  WAJZ-FILTRO.
                   15  WAJZ-FLT-TIPO-HIST
                                       PIC  X(001).
      *---             T = TODOS
      *---             P = PEDIDOS
      *---             D = DEFERIDOS NAO VIGENTES
                   15  WAJZ-FLT-CAGPTO-CTA
                                       PIC  9(003).
               10  WAJZ-PONTEIRO.
                   15  WAJZ-PTR-CSGL-UF
                                       PIC  X(002).
           05  WAJZ-ERRO               PIC  9(001).
           05  WAJZ-COD-MSG-ERRO       PIC  9(007).
           05  WAJZ-COD-SQL-ERRO       PIC  9(003).
           05  WAJZ-DESC-MSG-ERRO      PIC  X(079).
           05  WAJZ-FIM                PIC  X(001).
           05  FILLER                  PIC  X(329).
           05  WAJZ-DADOS              OCCURS 20.
               10  WAJZ-COD-UF         PIC  X(002).
               10  WAJZ-DESCR-UF       PIC  X(025).

