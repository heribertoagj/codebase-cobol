      *===============================================================*
      * INC          : I#RCOR31                                       *
      * DESCRICAO    : AREA DE COMUNICACAO PROGRAMA RCO5050           *
      * DATA CRIACAO : SET/2025                                       *
      * AUTOR        : 7COMM                                          *
      * ------------------------------------------------------------- *
      * ALTERACOES:                                                   *
      *    DATA           NOME                   DESCRICAO            *
      * ==========  =================  ============================== *
      * DD/MM/AAAA  XXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *===============================================================*
       01 RCOR31-REGISTRO.
          03 RCOR31-ENTRADA.
             05 RCOR31-E-TPCONS          PIC 9(001)  VALUE ZEROS.
      **        1 - DATA.
      **        2 - CONTRATO.
             05 RCOR31-E-DIA-INI         PIC X(002)  VALUE SPACES.
             05 RCOR31-E-MES-INI         PIC X(002)  VALUE SPACES.
             05 RCOR31-E-ANO-INI         PIC X(004)  VALUE SPACES.
             05 RCOR31-E-DIA-FIM         PIC X(002)  VALUE SPACES.
             05 RCOR31-E-MES-FIM         PIC X(002)  VALUE SPACES.
             05 RCOR31-E-ANO-FIM         PIC X(004)  VALUE SPACES.
          03 RCOR31-SAIDA.
             05 RCOR31-S-TOT-ERROS       PIC 9(003)  VALUE ZEROS.
             05 RCOR31-S-ERROS           OCCURS 620 TIMES.
                07  RCOR31-S-CODERRO     PIC X(008)  VALUE SPACES.
                07  RCOR31-S-REF         PIC X(011)  VALUE SPACES.
                07  RCOR31-S-DATA        PIC X(010)  VALUE SPACES.
                07  RCOR31-S-VALOR       PIC X(019)  VALUE SPACES.
             05 RCOR31-S-COD-RETORNO     PIC 9(005)  VALUE ZEROS.
