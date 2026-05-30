      *===============================================================*
      * INC          : I#RCOR12                                       *
      * DESCRICAO    : AREA PARA CONVERSAO DE UM TEXTO HEXADECIMAL EM *
      *                CARACTER                                       *
      * DATA CRIACAO : 01/08/2012                                     *
      * AUTOR        : PRIME INFORMATICA                              *
      * TAMANHO      : 534                                            *
      * ------------------------------------------------------------- *
      * ALTERACOES:                                                   *
      *    DATA           NOME                   DESCRICAO            *
      * ==========  =================  ============================== *
      * DD/MM/AAAA  XXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *===============================================================*
           02 COMMHEXBC-AREA.
              03 HEXBC-FUNCTION                 PIC X(04).
                 88 HEXBC-FUNCTION-VALID        VALUE 'BXBC', 'BXCB'.
                 88 HEXBC-FUNCTION-HEXA-TO-CHAR VALUE 'BXBC'.
                 88 HEXBC-FUNCTION-CHAR-TO-HEXA VALUE 'BXCB'.
              03 HEXBC-RETURN-CODE              PIC 9(02).
                 88 HEXBC-RC-OK                 VALUE ZEROES.
                 88 HEXBC-RC-INVALID-INPUT      VALUE 91.
                 88 HEXBC-RC-INVALID-FUNCTION   VALUE 99.
              03 HEXBC-REASON-CODE              PIC 9(08).
              03 HEXBC-DATA-LENGTH              PIC 9(008).
              03 HEXBC-DATA                     PIC X(512) VALUE SPACES.
