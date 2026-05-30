      *===============================================================*
      * IDENTIFICACAO:                                                *
      *                                                               *
      * NOME DA AREA DE COMUNICACAO ...: KCIMW184                     *
      * NOME DO PROGRAMA ..............: KCIM0184                     *
      *                                                               *
      * TAMANHO DA AREA EM BYTES ......: 10000 BYTES                  *
      *                                                               *
      * DATA DE CRIACAO ........: 23/12/2013                          *
      * ANALISTA RESPONSAVEL ...: RENATO FEITOSA E JAIR ALVES         *
      * PROGRAMADOR ............: BSI TECNOLOGIA                      *
      * --------------------------------------------------------------*
      * DESCRICAO:       AREA DE COMUNICACAO DO PROGRAMA KCIM0184.    *
      *                                                               *
      *===============================================================*
      * ALTERACOES:                                                   *
      *                                                               *
      * DATA        NOME       DESCRICAO                              *
      * =========== ========== ====================================== *
      *                                                               *
      *                                                               *
      * KCIMW184-E-FUNCAO                                             *
      *                                                               *
      * 01 - CONSULTA POR AGENCIA + PACB                              *
      *      RETORNA DADOS DA LOJA + NOME AGENCIA + NOME EMPRESA      *
      *      (APENAS 1 REGISTRO)                                      *
      *                                                               *
      * 02 - CONSULTA POR EMPRESA                                     *
      *      RETORNA DADOS DA LOJA + NOME AGENCIA + NOME EMPRESA      *
      *      (APENAS 1 REGISTRO DA PRIMEIRA LOJA ENCONTRADA)          *
      *                                                               *
      * 03 - CONSULTA POR LOJA + EMPRESA                              *
      *      RETORNA DADOS DA LOJA + NOME AGENCIA + NOME EMPRESA      *
      *      (APENAS 1 REGISTRO)                                      *
      *                                                               *
      * 04 - CONSULTA POR AGENCIA                                     *
      *      RETORNA DADOS DA LOJA + NOME AGENCIA + NOME EMPRESA      *
      *      (N REGISTROS)                                            *
      *                                                               *
      * 05 - CONSULTA POR EMPRESA                                     *
      *      RETORNA NOME DA EMPRESA                                  *
      *                                                               *
      * 06 - CONSULTA POR AGENCIA                                     *
      *      RETORNA NOME DA AGENCIA                                  *
      *      (APENAS 1 REGISTRO)                                      *
      *                                                               *
      *===============================================================*
       01  KCIMW184-AREA-COMUNICACAO.
           03  KCIMW184-E-ENTRADA.
               05  KCIMW184-E-FUNCAO                  PIC 9(02).
                   88  KCIMW184-E-FUNCAO-1            VALUE 01.
                   88  KCIMW184-E-FUNCAO-2            VALUE 02.
                   88  KCIMW184-E-FUNCAO-3            VALUE 03.
                   88  KCIMW184-E-FUNCAO-4            VALUE 04.
                   88  KCIMW184-E-FUNCAO-5            VALUE 05.
                   88  KCIMW184-E-FUNCAO-6            VALUE 06.
               05  KCIMW184-E-FILTRO-FUNCAO-1.
                   07  KCIMW184-E-CAG-BCRIA-1         PIC 9(05).
                   07  KCIMW184-E-CPOSTO-BCRIO-1      PIC 9(03).
               05  KCIMW184-E-FILTRO-FUNCAO-2.
                   07  KCIMW184-E-CEMPR-CORSP-2       PIC 9(10).
               05  KCIMW184-E-FILTRO-FUNCAO-3.
                   07  KCIMW184-E-CEMPR-CORSP-3       PIC 9(10).
               05  KCIMW184-E-FILTRO-FUNCAO-4.
                   07  KCIMW184-E-CAG-BCRIA-4         PIC 9(05).
                   07  KCIMW184-E-RESTART-4.
                       09  KCIMW184-E-CPOSTO-BCRIO-4  PIC 9(03).
               05  KCIMW184-E-FILTRO-FUNCAO-5.
                   07  KCIMW184-E-CEMPR-CORSP-5       PIC 9(10).
               05  KCIMW184-E-FILTRO-FUNCAO-6.
                   07  KCIMW184-E-CAG-BCRIA-6         PIC 9(05).
           03  KCIMW184-S-SAIDA.
               05  KCIMW184-S-COD-RETORNO             PIC 9(02).
                   88  KCIMW184-S-OK                  VALUE 00.
                   88  KCIMW184-S-ER-FUNCAO           VALUE 01.
                   88  KCIMW184-S-ER-CAG-BCRIA        VALUE 02.
                   88  KCIMW184-S-ER-CPOSTO-BCRIO     VALUE 03.
                   88  KCIMW184-S-ER-CEMPR-CORSP      VALUE 04.
                   88  KCIMW184-S-ER-CLJ-CORSP        VALUE 05.
                   88  KCIMW184-S-ER-INEXISTENTE      VALUE 98.
                   88  KCIMW184-S-ER-DB2              VALUE 99.
               05  KCIMW184-S-ER-DB2-AREA.
                   07  KCIMW184-S-ERRO-AREA           PIC X(107).
               05  KCIMW184-S-QTD-OCORRENCIAS         PIC 9(02).
               05  KCIMW184-S-IND-RESTART             PIC X(01).
                   88  KCIMW184-S-IND-RESTART-SIM     VALUE 'S'.
                   88  KCIMW184-S-IND-RESTART-NAO     VALUE 'N'.
               05  KCIMW184-S-OCORRENCIA OCCURS 20 TIMES.
                   07  KCIMW184-S-CAG-BCRIA           PIC S9(05) COMP-3.
                   07  KCIMW184-S-CPOSTO-BCRIO        PIC S9(03) COMP-3.
                   07  KCIMW184-S-IRZ-CORSP-BCRIO     PIC  X(70).
                   07  KCIMW184-S-ICORSP-BCRIO        PIC  X(70).
                   07  KCIMW184-S-CEMPR-CORSP         PIC S9(10) COMP-3.
                   07  KCIMW184-S-CLJ-CORSP           PIC  X(15).
                   07  KCIMW184-S-CSIT-CORSP-BCRIO    PIC S9(03) COMP-3.
                   07  KCIMW184-S-CPSSOA-JURID        PIC S9(10) COMP-3.
                   07  KCIMW184-S-NSEQ-UND-ORGNZ      PIC S9(08) COMP-3.
                   07  KCIMW184-S-CAG-PSTAL           PIC S9(10) COMP-3.
                   07  KCIMW184-S-CCNPJ-CORSP-BCRIO   PIC S9(09) COMP-3.
                   07  KCIMW184-S-CFLIAL-CORSP-BCRIO  PIC S9(05) COMP-3.
                   07  KCIMW184-S-CCTRL-CORSP-BCRIO   PIC S9(02) COMP-3.
                   07  KCIMW184-S-CTPO-CORSP-BCRIO    PIC S9(03) COMP-3.
                   07  KCIMW184-S-QDIA-VNCLO          PIC S9(02) COMP-3.
                   07  KCIMW184-S-CRGIAO-CORSP        PIC S9(05) COMP-3.
                   07  KCIMW184-S-CINDCD-RCOLH-NUMER  PIC S9(01) COMP-3.
                   07  KCIMW184-S-CEXTEN-DEPTO        PIC S9(05) COMP-3.
                   07  KCIMW184-S-CTARIF-REPAS        PIC S9(05) COMP-3.
                   07  KCIMW184-S-CLOGDR-COMPE        PIC S9(03) COMP-3.
                   07  KCIMW184-S-CTPO-FUNCN          PIC S9(02) COMP-3.
                   07  KCIMW184-S-HINIC-OPER          PIC  X(26).
                   07  KCIMW184-S-HULT-ALT            PIC  X(26).
                   07  KCIMW184-S-HALT-UND-ORGNZ      PIC  X(26).
                   07  KCIMW184-S-NTMNHO-COMPV-GERDR  PIC S9(03) COMP-3.
                   07  KCIMW184-S-CINDCD-BLOQ-EMPR    PIC S9(01) COMP-3.
                   07  KCIMW184-S-CINDCD-BLOQ-LJ      PIC S9(01) COMP-3.
                   07  KCIMW184-S-NM-AGENCIA          PIC  X(70).
                   07  KCIMW184-S-NM-EMPRESA          PIC  X(70).
           03  KCIMW184-RESERVA                       PIC X(964).
