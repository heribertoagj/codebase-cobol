      *****************************************************************
      * NOME DA INC - I#GFCTON                                        *
      * DESCRICAO   - COMBO DE AGRUPAMENTO COM FLEXIBILIZACAO         *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * DATA        - 01.04.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352 R 208 *
      *****************************************************************
      *
       01  GFCTON-SAIDA.
           03  GFCTON-LL                            PIC S9(004) COMP.
           03  GFCTON-ZZ                            PIC S9(004) COMP.
           03  GFCTON-TRANSACAO                     PIC  X(009).
           03  GFCTON-FUNCAO                        PIC  X(008).
           03  GFCTON-FILTRO                        PIC  9(003).
           03  GFCTON-QTDE-OCOR                     PIC  9(003).
           03  GFCTON-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTON-FUNC-BDSCO                    PIC  X(007).
           03  GFCTON-PONTEIRO.
               05  GFCTON-PONTEIRO-AGRUP            PIC  9(003).
           03  GFCTON-ERRO                          PIC  9(001).
           03  GFCTON-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTON-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTON-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTON-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(026).
           03  GFCTON-DADOS.
               05  FILLER                           OCCURS 25.
                   07  GFCTON-COD-AGRUP             PIC  9(003).
                   07  GFCTON-DESC-AGRUP            PIC  X(030).
080708             07  GFCTON-PERSONALIZ            PIC  X(001).
