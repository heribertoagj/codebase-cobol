      *===============================================================*
      * INC          : I#RCOR09                                       *
      * DESCRICAO    : LAYOUT P/ ENVIO DAS MENSAGENS DE RESPOSTA DO   *
      *                TMNF E BACEN PARA OS LEGADOS                   *
      * DATA CRIACAO : AGO/2012                                       *
      * AUTOR        : PRIME INFORMATICA                              *
      * TAMANHO      : 30040                                          *
      * ------------------------------------------------------------- *
      * ALTERACOES:                                                   *
      *    DATA           NOME                   DESCRICAO            *
      * ==========  =================  ============================== *
      * DD/MM/AAAA  XXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *===============================================================*
       01 RCOR09-REGISTRO.
          03 RCOR09-ENTRADA.
             05 RCOR09-COD-MSG         PIC X(009).
             05 RCOR09-NRO-CTRL-IF     PIC X(020).
             05 RCOR09-TAM-SISMSG      PIC 9(005)  COMP-3.
             05 RCOR09-SEQ-MSG         PIC 9(003).
             05 RCOR09-SISMSG          PIC X(30000).
          03 RCOR09-SAIDA.
             05 RCOR09-COD-RETORNO     PIC 9(005).
