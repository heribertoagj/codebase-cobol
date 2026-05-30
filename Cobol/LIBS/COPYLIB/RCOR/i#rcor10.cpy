      *===============================================================*
      * INC          : I#RCOR10                                       *
      * DESCRICAO    : LAYOUT MODULOS RCOR7000/RCOR7005 PARA ENVIO    *
      *                DAS MENSAGEN COR0001/COR0002 PARA O TMNF       *
      * DATA CRIACAO : AGO/2012                                       *
      * AUTOR        : PRIME INFORMATICA                              *
      * TAMANHO      : 30057                                          *
      * ------------------------------------------------------------- *
      * ALTERACOES:                                                   *
      *    DATA           NOME                   DESCRICAO            *
      * ==========  =================  ============================== *
      * DD/MM/AAAA  XXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *===============================================================*
       01 RCOR10-REGISTRO.
          03 RCOR10-ENTRADA.
             05 RCOR10-CCUSTO          PIC  X(00004).
             05 RCOR10-TAM-SISMSG      PIC S9(00008) COMP.
             05 RCOR10-SEQ-MSG         PIC  9(00003).
             05 RCOR10-IND-CONTINUA    PIC  X(00001).
      **        N - NAO
             05 RCOR10-NRO-CTRLIF      PIC  X(00020).
             05 RCOR10-SISMSG          PIC  X(30000).
          03 RCOR10-SAIDA.
             05 RCOR10-NRO-CTRL-IF     PIC  X(00020).
             05 RCOR10-COD-RETORNO     PIC  9(00005).
