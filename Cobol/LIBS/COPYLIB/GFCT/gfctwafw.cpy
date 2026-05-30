      ******************************************************************
      * NOME DA INC - GFCTWAFW                                         *
      * DESCRICAO   - COMBO DE TARIFA P/ EXCLUSAO MASSIVA ADESAO PACOTE*
      *               COMUNICACAO IMS X WEB - SAIDA                    *
      * TAMANHO     - 1010                                             *
      * DATA        - 16/02/2009                                       *
      * RESPONSAVEL - WAGNER SILVA    - PROCWORK - TEL 4506-2345       *
      ******************************************************************
      *
       01  WAFW-SAIDA.
           03  WAFW-LL                              PIC S9(004) COMP.
           03  WAFW-ZZ                              PIC S9(004) COMP.
           03  WAFW-TRANSACAO                       PIC  X(009).
           03  WAFW-FUNCAO                          PIC  X(008).
           03  WAFW-FILTRO.
               05 WAFW-COD-AGRUPAMENTO              PIC  9(003).
           03  WAFW-QTDE-OCOR                       PIC  9(003).
           03  WAFW-QTDE-TOT-REG                    PIC  9(006).
           03  WAFW-FUNC-BDSCO                      PIC  X(007).
           03  WAFW-TERMINAL                        PIC  X(008).
           03  WAFW-PONTEIRO.
               05 WAFW-PONTEIRO-TARIFA              PIC  9(005).
           03  WAFW-ERRO                            PIC  9(001).
           03  WAFW-COD-MSG-ERRO                    PIC  9(007).
           03  WAFW-COD-SQL-ERRO                    PIC  9(003).
           03  WAFW-DESC-MSG-ERRO                   PIC  X(079).
           03  WAFW-FIM                             PIC  X(001).
           03  WAFW-TIPO-FUNCAO                     PIC  9(001).
      *        1 - DEFERIMENTO
      *        2 - CONSULTA
      *        3 - EXCLUSAO
           03  FILLER                               PIC  X(025).
           03  WAFW-DADOS.
               05  FILLER                           OCCURS 8.
                   07  WAFW-COD-TARIFA              PIC  9(005).
                   07  WAFW-DESC-TARIFA             PIC  X(100).
