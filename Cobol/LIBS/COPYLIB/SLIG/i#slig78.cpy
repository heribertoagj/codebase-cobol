      ***===========================================================***
      *** NOME INC                                     LENGTH=00180 ***
      *** I#SLIG78                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INTERMEDIARIO CONCENTRACAO DIARIA      ***
      ***            PRODUTO CHEQUE                                 ***
      ***                                                           ***
      ***===========================================================***
       01  REG-INTER-COGEN-CHEQUE.
           03 SLIG78-BCO-EMITT              PIC 9(003) COMP-3.
           03 SLIG78-AGENCIA-EMITT          PIC 9(005) COMP-3.
           03 SLIG78-CONTA-EMITT            PIC 9(013) COMP-3.
           03 SLIG78-CGC-CPF-EMITT          PIC 9(009) COMP-3.
           03 SLIG78-FILIAL-EMITT           PIC 9(004) COMP-3.
           03 SLIG78-CONTROLE-EMITT         PIC 9(002) COMP-3.
           03 SLIG78-TIPO-PESSOA-EMITT      PIC X(001).
           03 SLIG78-NOME-EMITT             PIC X(40).
           03 SLIG78-BCO-CLI                PIC 9(003) COMP-3.
           03 SLIG78-AGENCIA-CLI            PIC 9(005) COMP-3.
           03 SLIG78-CONTA-CLI              PIC 9(013) COMP-3.
           03 SLIG78-CGC-CPF-CLI            PIC 9(009) COMP-3.
           03 SLIG78-FILIAL-CLI             PIC 9(004) COMP-3.
           03 SLIG78-CONTROLE-CLI           PIC 9(002) COMP-3.
           03 SLIG78-TIPO-PESSOA-CLI        PIC X(001).
           03 SLIG78-NOME-CLI               PIC X(40).
           03 SLIG78-CD-CART-CLI            PIC 9(003) COMP-3.
           03 SLIG78-VL-ABERTO-CLI          PIC 9(013)V99 COMP-3.
           03 SLIG78-VL-ABERTO-SAC-SIMP     PIC 9(013)V99 COMP-3.
           03 SLIG78-VL-ABERTO-SAC-CAUC     PIC 9(013)V99 COMP-3.
           03 SLIG78-VL-ABERTO-SAC-DESC     PIC 9(013)V99 COMP-3.
      *
      *    IDENTIFICA NOME DO EMITENTE COLETADO DO SERASA
           03 SLIG78-NOME-EMITT-SERASA      PIC X(01).

      * === CAMPOS DUPLICADOS PARA STRING ===
           03 SLIG78-CGC-CPF-EMITT-ST       PIC X(009).
           03 SLIG78-FILIAL-EMITT-ST        PIC X(004).
           03 SLIG78-CONTROLE-EMITT-ST      PIC 9(002) COMP-3.
           03 SLIG78-CGC-CPF-CLI-ST         PIC X(009).
           03 SLIG78-FILIAL-CLI-ST          PIC X(004).
           03 SLIG78-CONTROLE-CLI-ST        PIC 9(002) COMP-3.
           03 SLIG78-FILLER                 PIC X(19).
