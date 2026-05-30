      ******************************************************************
      * NOME DA INC - GFCTWAJV                                         *
      * DESCRICAO   - COMUNICACAO WEB X IMS                 - SAIDA    *
      *               FLEXIBILIZACAO PARA AGRUPAMENTO                  *
      *               CONSULTA HISTORICOS                              *
      *               COMBO SEGMENTO HISTORICO FLEXIBILIZ   - GFCT5179 *
      * TAMANHO     - 1010 - TOTAL                                     *
      *               0350 - FIXO                                      *
      *               0660 - VARIAVEL (33 X 20)                        *
      * DATA        - 27.11.2009                                       *
      * RESPONSAVEL - FRANCISCO LOURENCO - SONDAPROCWORK               *
      ******************************************************************
      *
       01  WAJV-SAIDA.
           05  WAJV-LL                 PIC S9(004) COMP.
           05  WAJV-ZZ                 PIC S9(004) COMP.
           05  WAJV-TRANSACAO          PIC  X(009).
           05  WAJV-FUNCAO             PIC  X(008).
           05  WAJV-DADOS-RST.
               10  WAJV-QTDE-OCOR      PIC  9(007).
               10  WAJV-QTDE-TOT-REG   PIC  9(009).
               10  WAJV-FUNC-BDSCO     PIC  X(007).
               10  WAJV-FILTRO.
                   15  WAJV-FLT-TIPO-HIST
                                       PIC  X(001).
      *---             T = TODOS
      *---             P = PEDIDOS
      *---             D = DEFERIDOS NAO VIGENTES
                   15  WAJV-FLT-CAGPTO-CTA
                                       PIC  9(003).
               10  WAJV-PONTEIRO.
                   15  WAJV-PTR-CSGMTO-TARIF
                                       PIC  9(003).
           05  WAJV-ERRO               PIC  9(001).
           05  WAJV-COD-MSG-ERRO       PIC  9(007).
           05  WAJV-COD-SQL-ERRO       PIC  9(003).
           05  WAJV-DESC-MSG-ERRO      PIC  X(079).
           05  WAJV-FIM                PIC  X(001).
           05  FILLER                  PIC  X(208).
           05  WAJV-DADOS              OCCURS 20.
               10  WAJV-COD-SEGMTO     PIC  9(003).
               10  WAJV-DESCR-SEGMTO   PIC  X(030).
