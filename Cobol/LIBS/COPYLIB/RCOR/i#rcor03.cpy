      *===============================================================*
      * INC          : I#RCOR03                                       *
      * DESCRICAO    : LAYOUT BLOCO DE RETORNO ENVIADO PELO SISTEMA   *
      *                TMNF PARA O RCOR                               *
      * DATA CRIACAO : AGO/2012                                       *
      * AUTOR        : PRIME INFORMATICA                              *
      * TAMANHO      : 217                                            *
      * ------------------------------------------------------------- *
      * ALTERACOES:                                                   *
      *    DATA           NOME                   DESCRICAO            *
      * ==========  =================  ============================== *
      * DD/MM/AAAA  XXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *===============================================================*
       01 RCOR03-REGISTRO.
          03 RCOR03-BLOCO-RETORNO.
             05 RCOR03-ID-BLOCO           PIC X(008).
             05 RCOR03-CNRO-CTRL-INSTC    PIC X(020).
             05 RCOR03-CNRO-UNIC          PIC X(023).
             05 RCOR03-CRETOR-PRINC       PIC 9(004).
             05 RCOR03-CRETOR-OPCAO       PIC 9(004).
             05 RCOR03-CNRO-UNIC-OPER     PIC X(023).
             05 RCOR03-CCTRL-ORIGE        PIC X(030).
             05 RCOR03-RMSGEM-ERRO        PIC X(080).
             05 RCOR03-RESERVADO          PIC X(023).
             05 FILLER                    PIC X(002).
