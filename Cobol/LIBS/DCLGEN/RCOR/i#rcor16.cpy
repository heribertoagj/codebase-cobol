      *===============================================================*
      * INC          : I#RCOR16                                       *
      * DESCRICAO    : LAYOUT RECEPCAO MENSAGENS DE RETORNO TMNF E    *
      *                BACEN                                          *
      * DATA CRIACAO : AGO/2012                                       *
      * AUTOR        : PRIME INFORMATICA                              *
      * TAMANHO      : 30418                                          *
      * ------------------------------------------------------------- *
      * ALTERACOES:                                                   *
      *    DATA           NOME                   DESCRICAO            *
      * ==========  =================  ============================== *
      * DD/MM/AAAA  XXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *===============================================================*
       01 RCOR16-REGISTRO.
          05 RCOR16-A-AREA-MSG             PIC X(30000) VALUE SPACES.
          05 RCOR16-S-RETORNO.
             07 RCOR16-S-CD-RETORNO        PIC 9(00003) VALUE ZEROS.
             07 RCOR16-S-MENSAGEM          PIC X(00079) VALUE SPACES.
             07 RCOR16-S-SQLCA             PIC X(00136) VALUE SPACES.
          05 FILLER                        PIC X(00200) VALUE SPACES.
