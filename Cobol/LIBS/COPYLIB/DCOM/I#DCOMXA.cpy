      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMXA *
      * CRIACAO   : 08/2007                                   *
      * DESCRICAO : ALTERACAO DE OPERACAO                     *
      *                                                       *
      * APLICACAO : MODULO DCOM5388                           *
      *                                                       *
      * TAMANHO   :                                           *
      *                                                       *
      *********************************************************
      *                                                       *
      * OBS: QUANDO DCOM5388 IDENTIFICA RECEBIMENTO DO CAMPO  *
      *      SOLICITA-ESTORNO COM CONTEUDO 'S' SOMENTE SERAH  *
      *      NECESSARIO TRAFEGAR ANO/SEQUENCIA DA OPERACAO.   *
      *      TRATA-SE DE ROGULARIZAR A OPERACAO QUANTO AO     *
      *      CONE, CONTRATO ,CONVENIO, ETC PARA FUTURA NOVA   *
      *      EFETIVACAO, POIS RETROAGE SITUACAO DA OPERACAO   *
      *      PARA 002                                         *
      *      ESTE CAMPO COM CONTEUDO 'N' TODO O LAYOUT SERAH  *
      *      CRITICADO PARA EFETIVAMENTE OCORRE A ALTERACAO   *
      *      DOS CAMPOS                                       *
      *********************************************************
      * AREA DE ENTRADA                                       *
      *********************************************************

       01  5388-ENTRADA.
           03  5388-COD-RETORNO                PIC  X(004).
           03  5388-MSG-RETORNO                PIC  X(079).
           03  5388-RESTART                    PIC  9(005).
           03  5388-FLAG                       PIC  X(001).
           03  5388-CFUNC-BDSCO                PIC  9(009).
           03  5388-CTERM                      PIC  X(008).
           03  5388-DADOS.
               05  5388-DANO-OPER-DESC         PIC  9(004).
               05  5388-NSEQ-OPER-DESC         PIC  9(009).
               05  5388-CPRODT                 PIC  9(003).
               05  5388-CSPROD-DESC-COML       PIC  9(003).
               05  5388-CAG-BCRIA              PIC  9(005).
               05  5388-CCTA-BCRIA-CLI         PIC  9(013).
               05  5388-CCNPJ-CPF              PIC  9(009).
               05  5388-CFLIAL-CNPJ            PIC  9(005).
               05  5388-CCTRL-CNPJ-CPF         PIC  9(002).
               05  5388-CONVENIO               PIC  9(009).
               05  5388-CONTRATO               PIC  9(009).
               05  5388-CCART-LIM-CREDT        PIC  X(005).
               05  5388-NCONSULTA.
                   10  5388-DANO-BASE          PIC  9(004).
                   10  5388-CDEPDC             PIC  9(005).
                   10  5388-CSEQ-STUDO         PIC  9(009).
               05  5388-VLR-SLD-DISP-LIM       PIC  9(015)V9(2).
DTS001         05  5388-VLR-SLD-DISP-PLUS      PIC  9(015)V9(2).
               05  5388-VOPER-DESC-COML        PIC  9(015)V9(2).
               05  5388-QTITLO-CHEQ-PCELA      PIC  9(009).
               05  5388-DINIC-OPER-DESC        PIC  X(010).
               05  5388-DVCTO-FNAL-OPER        PIC  X(010).
               05  5388-CMEIO-ENTRD-DESC       PIC  9(003).
               05  5388-BCO-EMITENTE           PIC  9(003).
               05  5388-AGENC-EMITENTE         PIC  9(005).
               05  5388-CCTA-EMITENTE          PIC  9(013).
WPG            05  5388-SOLICITA-ESTORNO       PIC  X(001).
DAN            05  5388-ESPECIE-TITLO          PIC  9(003).

       01  5388-SAIDA.
           03  5388-HEADER.
               05  5388-COD-RETORNO-S          PIC  X(004).
               05  5388-MSG-RETORNO-S          PIC  X(079).
               05  5388-RESTART-S              PIC  9(005).
               05  5388-FLAG-S                 PIC  X(001).

           03  5388-DADOS-S.
               05  5388-DANO-OPER-DESC-S       PIC  9(004).
               05  5388-NSEQ-OPER-DESC-S       PIC  9(009).
               05  5388-CCART-LIM-CREDT-S      PIC  X(005).
               05  5388-NCONSULTA-S.
                   10  5388-DANO-BASE-S        PIC  9(004).
                   10  5388-CDEPDC-S           PIC  9(005).
                   10  5388-CSEQ-STUDO-S       PIC  9(009).
               05  5388-VLR-PRINC-LMTE-S       PIC  9(015)V99.
               05  5388-SLD-DISPO-LMTE-S       PIC  9(015)V99.
               05  5388-VOPER-DESC-COML-S      PIC  9(015)V99.
               05  5388-QTITLO-CHEQ-PCELA-S    PIC  9(009).
               05  5388-DINIC-OPER-DESC-S      PIC  X(010).
               05  5388-DVCTO-FNAL-OPER-S      PIC  X(010).
               05  5388-CMEIO-ENTRD-DESC-S     PIC  9(003).

           03  5388-CONSISTENCIA.
               05  5388-TAMANHO-ERRO           PIC  9(001)      VALUE 2.
               05  5388-TABELA-ERROS    OCCURS  17  TIMES.
                   10  5388-CAMPOS-ERRO        PIC  9(002).

