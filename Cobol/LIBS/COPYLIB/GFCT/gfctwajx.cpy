      ******************************************************************
      * NOME DA INC - GFCTWAJX                                         *
      * DESCRICAO   - COMUNICACAO WEB X IMS                 - SAIDA    *
      *               FLEXIBILIZACAO PARA AGRUPAMENTO                  *
      *               CONSULTA HISTORICOS                              *
      *               COMBO UF FLEXIBILIZACAO VIGENTE       - GFCT5183 *
      * TAMANHO     - 1010 - TOTAL                                     *
      *               0470 - FIXO                                      *
      *               0540 - VARIAVEL (25 X 20)                        *
      * DATA        - 27.11.2009                                       *
      * RESPONSAVEL - FRANCISCO LOURENCO - SONDAPROCWORK               *
      ******************************************************************
      *
       01  WAJX-SAIDA.
           05  WAJX-LL                 PIC S9(004) COMP.
           05  WAJX-ZZ                 PIC S9(004) COMP.
           05  WAJX-TRANSACAO          PIC  X(009).
           05  WAJX-FUNCAO             PIC  X(008).
           05  WAJX-DADOS-RST.
               10  WAJX-QTDE-OCOR      PIC  9(007).
               10  WAJX-QTDE-TOT-REG   PIC  9(009).
               10  WAJX-FUNC-BDSCO     PIC  X(007).
               10  WAJX-FILTRO.
                   15  WAJX-FLT-CAGPTO-CTA
                                       PIC  9(003).
               10  WAJX-PONTEIRO.
                   15  WAJX-PTR-CSGL-UF
                                       PIC  X(002).
           05  WAJX-ERRO               PIC  9(001).
           05  WAJX-COD-MSG-ERRO       PIC  9(007).
           05  WAJX-COD-SQL-ERRO       PIC  9(003).
           05  WAJX-DESC-MSG-ERRO      PIC  X(079).
           05  WAJX-FIM                PIC  X(001).
           05  FILLER                  PIC  X(330).
           05  WAJX-DADOS              OCCURS 20.
               10  WAJX-COD-UF         PIC  X(002).
               10  WAJX-DESCR-UF       PIC  X(025).
