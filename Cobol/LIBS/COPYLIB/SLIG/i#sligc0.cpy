      ***===========================================================***
      *** NOME INC                                     LENGTH=01050 ***
      *** I#SLIGC0                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            NOVO LAY-OUT HISTORICO DE CHEQUES              ***
      ***            ARQUIVO CHEQUE - DIARIA E DECENAL              ***
      ***                                                           ***
      *** OBS: ESTA INC TEM COMO BASE AS INCS I#SLIG27 E I#SLIG28   ***
      ***                                                           ***
      *** CHEQUES  : NORMAL, EXCLUIDOS, COMPENSADOS,                ***
      ***            REAPRESENTADOS E DEVOLVIDOS                    ***
      ***-----------------------------------------------------------***
      *
       01  REG-CHEQUE-OCOR-HISTORICA.
           03 SLIGC0-EMITENTE.
              05 SLIGC0-CGC-CPF-EMITT       PIC 9(009) COMP-3.
              05 SLIGC0-FILIAL-EMITT        PIC 9(004) COMP-3.
              05 SLIGC0-CONTROLE-EMITT      PIC 9(002) COMP-3.
              05 SLIGC0-TIPO-PESSOA-EMITT   PIC X(001).
              05 SLIGC0-NOME-EMITT          PIC X(040).
              05 SLIGC0-BCO-EMITT           PIC 9(003) COMP-3.
              05 SLIGC0-AGENCIA-EMITT       PIC 9(005) COMP-3.
              05 SLIGC0-CONTA-EMITT         PIC 9(013) COMP-3.
              05 SLIGC0-GRP-ECONOMICO-EMITT PIC 9(009) COMP-3.
           03 SLIGC0-CLIENTE.
              05 SLIGC0-CGC-CPF-CLI         PIC 9(009) COMP-3.
              05 SLIGC0-FILIAL-CLI          PIC 9(004) COMP-3.
              05 SLIGC0-CONTROLE-CLI        PIC 9(002) COMP-3.
              05 SLIGC0-TIPO-PESSOA-CLI     PIC X(001).
              05 SLIGC0-NOME-CLI            PIC X(040).
              05 SLIGC0-BCO-CLI             PIC 9(003) COMP-3.
              05 SLIGC0-AGENCIA-CLI         PIC 9(005) COMP-3.
              05 SLIGC0-CONTA-CLI           PIC 9(013) COMP-3.
              05 SLIGC0-GRP-ECONOMICO-CLI   PIC 9(009) COMP-3.
           03 SLIGC0-CHEQUE.
              05 SLIGC0-NUM-CHEQUE          PIC 9(007) COMP-3.
              05 SLIGC0-DIG-CHEQUE          PIC X(001).
              05 SLIGC0-NUM-LOTE            PIC 9(007) COMP-3.
              05 SLIGC0-CARTEIRA            PIC 9(003) COMP-3.
              05 SLIGC0-VL-PRINC-CHEQ       PIC 9(013)V99 COMP-3.
              05 SLIGC0-BCO-DIR-REGIONAL    PIC 9(003) COMP-3.
              05 SLIGC0-DIR-REGIONAL        PIC 9(005) COMP-3.
              05 SLIGC0-BCO-GERC-REGIONAL   PIC 9(003) COMP-3.
              05 SLIGC0-GERC-REGIONAL       PIC 9(005) COMP-3.
              05 SLIGC0-QT-OCCORS           PIC 9(003) COMP-3.
           03 SLIGC0-HISTORICO-CHEQUE       OCCURS 20 TIMES.
              05 SLIGC0-CTPO-CHEQ           PIC 9(003) COMP-3.
              05 SLIGC0-CODI-DEVOL-CHQ      PIC 9(002).
              05 SLIGC0-DT-MOVIMENTO        PIC 9(008).
              05 SLIGC0-DT-MOVIMENTO-RED    REDEFINES
                 SLIGC0-DT-MOVIMENTO.
                 07 SLIGC0-ANO-MOVIMENTO    PIC 9(004).
                 07 SLIGC0-MES-MOVIMENTO    PIC 9(002).
                 07 SLIGC0-DIA-MOVIMENTO    PIC 9(002).
              05 SLIGC0-DT-DEPOSITO         PIC 9(008).
              05 SLIGC0-DT-DEPOSITO-RED     REDEFINES
                 SLIGC0-DT-DEPOSITO.
                 07 SLIGC0-ANO-DEPOSITO     PIC 9(004).
                 07 SLIGC0-MES-DEPOSITO     PIC 9(002).
                 07 SLIGC0-DIA-DEPOSITO     PIC 9(002).
              05 SLIGC0-DT-CONTABIL         PIC 9(008).
              05 SLIGC0-DT-CONTABIL-RED     REDEFINES
                 SLIGC0-DT-CONTABIL.
                 07 SLIGC0-ANO-CONTABIL     PIC 9(004).
                 07 SLIGC0-MES-CONTABIL     PIC 9(002).
                 07 SLIGC0-DIA-CONTABIL     PIC 9(002).
              05 SLIGC0-DT-ENTRADA-ORIGEM   PIC 9(008).
              05 SLIGC0-DT-ENTRADA-ORIGEM-RED REDEFINES
                 SLIGC0-DT-ENTRADA-ORIGEM.
                 07 SLIGC0-ANO-ENTRADA-ORIGEM PIC 9(004).
                 07 SLIGC0-MES-ENTRADA-ORIGEM PIC 9(002).
                 07 SLIGC0-DIA-ENTRADA-ORIGEM PIC 9(002).
              05 SLIGC0-DT-SITUACAO         PIC 9(008).
              05 SLIGC0-DT-SITUACAO-RED     REDEFINES
                 SLIGC0-DT-SITUACAO.
                 07 SLIGC0-ANO-SITUACAO     PIC 9(004).
                 07 SLIGC0-MES-SITUACAO     PIC 9(002).
                 07 SLIGC0-DIA-SITUACAO     PIC 9(002).
      *
      *    IDENTIFICA NOME DO EMITENTE COLETADO DO SERASA
           03 SLIGC0-NOME-EMITT-SERASA      PIC X(001).
           03 SLIGC0-FILLER                 PIC X(002).

      ***===========================================================***
      ***                                                           ***
      ***  VALORES PARA SLIGC0-CTPO-CHEQ                            ***
      ***                                                           ***
      ***     EXCLUIDO                   = 001                      ***
      ***     COMPENSADO/PAGO            = 002                      ***
      ***     REAPRESENTADO              = 003                      ***
      ***     NORMAL                     = 004                      ***
      ***     DEVOLVIDO                  = 005                      ***
      ***                                                           ***
      ***===========================================================***

      * === CAMPOS DUPLICADOS PARA STRING ===
           03 SLIGC0-CGC-CPF-EMITT-ST       PIC X(009).
           03 SLIGC0-FILIAL-EMITT-ST        PIC X(004).
           03 SLIGC0-CONTROLE-EMITT-ST      PIC 9(002) COMP-3.
           03 SLIGC0-CGC-CPF-CLI-ST         PIC X(009).
           03 SLIGC0-FILIAL-CLI-ST          PIC X(004).
           03 SLIGC0-CONTROLE-CLI-ST        PIC 9(002) COMP-3.
