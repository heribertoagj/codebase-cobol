      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW022                                           *
      * DESCRICAO : LAYOUT DO MODULO BVVE8003                          *
      * DATA      : 20/01/2014                                         *
      * AUTOR     : FABRICIO MASAKIYO MANCINI                          *
      * EMPRESA   : 7COMM                                              *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 23000 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      *
           05 BVVE22-REGISTRO.
             10 BVVE22-FASE                    PIC  9(001).
             10 BVVE22-DADOS.
               15 BVVE22-EIBTRMID              PIC  X(008).
               15 BVVE22-CPROG-ORIGE           PIC  X(008).
               15 BVVE22-CBCO-CHAVE            PIC  9(003).
               15 BVVE22-CBCO-INICIO           PIC  9(003).
               15 BVVE22-CBCO-FIM              PIC  9(003).
               15 BVVE22-CEMISR-CHAVE          PIC  X(008).
               15 BVVE22-CEMISR-INICIO         PIC  X(008).
               15 BVVE22-CEMISR-FIM            PIC  X(008).
               15 BVVE22-CMSG-CHAVE            PIC  X(009).
               15 BVVE22-CMSG-INICIO           PIC  X(009).
               15 BVVE22-CMSG-FIM              PIC  X(009).
               15 BVVE22-CDSTNA-CHAVE          PIC  X(008).
               15 BVVE22-CDSTNA-INICIO         PIC  X(008).
               15 BVVE22-CDSTNA-FIM            PIC  X(008).
               15 BVVE22-LEGADO-CHAVE          PIC  X(004).
               15 BVVE22-LEGADO-INICIO         PIC  X(004).
               15 BVVE22-LEGADO-FIM            PIC  X(004).
               15 BVVE22-DPLAVL-CHAVE          PIC S9(001) COMP-3.
               15 BVVE22-DPLAVL-INICIO         PIC S9(001) COMP-3.
               15 BVVE22-DPLAVL-FIM            PIC S9(001) COMP-3.
               15 BVVE22-NCONTR-CHAVE          PIC  X(020).
               15 BVVE22-NCONTR-INICIO         PIC  X(020).
               15 BVVE22-NCONTR-FIM            PIC  X(020).
               15 BVVE22-NCONTR-BACEN-CHAVE    PIC  X(023).
               15 BVVE22-NCONTR-BACEN-INICIO   PIC  X(023).
               15 BVVE22-NCONTR-BACEN-FIM      PIC  X(023).
               15 BVVE22-STATUS-CHAVE          PIC S9(005) COMP-3.
               15 BVVE22-STATUS-INICIO         PIC S9(005) COMP-3.
               15 BVVE22-STATUS-FIM            PIC S9(005) COMP-3.
               15 BVVE22-HOPER-ENVIO-CHAVE     PIC  X(026).
               15 BVVE22-OCORR-HOPER       OCCURS  12 TIMES.
                   20 BVVE22-HOPER-ENVIO       PIC  X(026).
               15 BVVE22-DMOVTO-OPER-CHAVE     PIC  X(010).
               15 BVVE22-DMOVTO-OPER-INICIO    PIC  X(010).
               15 BVVE22-DMOVTO-OPER-FIM       PIC  X(010).
               15 BVVE22-OCORR-DMOVTO      OCCURS 12 TIMES.
                   20 BVVE22-DMOVTO-OPER       PIC  X(010).
               15 BVVE22-TPENV-CHAVE           PIC  X(001).
               15 BVVE22-TPENV-INICIO          PIC  X(001).
               15 BVVE22-TPENV-FIM             PIC  X(001).
               15 BVVE22-TITULO                PIC  X(020).
               15 BVVE22-PAGINA                PIC  9(003).
               15 BVVE22-FIM-LISTA             PIC  X(001).
               15 BVVE22-NSEQ-FLUXO-MSG-INIC   PIC  9(005).
               15 BVVE22-NSEQ-FLUXO-MSG-FIM    PIC  9(005).
               15 BVVE22-CUSUAR-SENHA          PIC  X(007).
               15 BVVE22-QTD-ENVIADAS          PIC  9(002).
               15 BVVE22-IND-PESQUISA          PIC  X(001).
               15 BVVE22-IND-TABELA            PIC  X(001).
               15 BVVE22-QTD-OCORRENCIAS       PIC  9(002).
               15 BVVE22-CPROG-CHAM-SYSMSG     PIC  X(008).
               15 BVVE22-PAG-SYSMSG            PIC  9(003).
               15 BVVE22-FASE-SYSMSG           PIC  9(001).
               15 BVVE22-ATUALIZAR-TABELAS     PIC  X(001).
               15 BVVE22-FIM-SPBLB002          PIC  X(001).
               15 BVVE22-NORD-CPO-REPET        PIC  X(352).
               15 BVVE22-CTRANS-ORIGE          PIC  X(004).
               15 BVVE22-NORD-REPET-NIVEL1     PIC  9(003).
               15 BVVE22-NORD-REPET-NIVEL2     PIC  9(003).
               15 BVVE22-NORD-REPET-NIVEL3     PIC  9(003).
               15 BVVE22-NORD-REPET-NIVEL4     PIC  9(003).
               15 BVVE22-DESCR-MSG             PIC  X(055).
               15 BVVE22-NIVEL1-REPET          PIC  9(004).
               15 BVVE22-NIVEL2-REPET          PIC  9(004).
               15 BVVE22-NIVEL3-REPET          PIC  9(004).
               15 BVVE22-NIVEL4-REPET          PIC  9(004).
               15 BVVE22-OCOR-NIVEL1           PIC  9(003).
               15 BVVE22-OCOR-NIVEL2           PIC  9(003).
               15 BVVE22-OCOR-NIVEL3           PIC  9(003).
               15 BVVE22-OCOR-NIVEL4           PIC  9(003).
               15 BVVE22-IND-NIVEL             PIC  9(001).
               15 BVVE22-DESCR-REPET-NIVEL1    PIC  X(037).
               15 BVVE22-DESCR-REPET-NIVEL2    PIC  X(037).
               15 BVVE22-DESCR-REPET-NIVEL3    PIC  X(037).
               15 BVVE22-DESCR-REPET-NIVEL4    PIC  X(037).
               15 BVVE22-NIVEL-REPET-UTILIZ    PIC  9(004).
               15 BVVE22-PRI-NORD-CPO-MSGEM-VOL PIC 9(003).
               15 BVVE22-OCORR-NSEQ-FLUXO-MSGEM OCCURS 12 TIMES.
                   20 BVVE22-NSEQ-FLUXO-MSGEM  PIC  9(005).
               15 BVVE22-NSEQ-CHAVE            PIC  9(005).
               15 BVVE22-CODIGO-FLUXO          PIC  9(004).
               15 BVVE22-IND                   PIC  9(002).
               15 BVVE22-FLUXO-MSGEM           PIC  X(001).
               15 BVVE22-MSGRIA                PIC  9(003).
      *                                                                *
             10 BVVE22-TABELA-B012.
               15 BVVE22-CEMPR-INC             PIC  9(003).
               15 BVVE22-CCTRO-CUSTO           PIC  X(004).
               15 BVVE22-CDEPDC                PIC  9(005).
               15 BVVE22-EVENTO.
                  20 BVVE22-CSIST-EXTER        PIC  X(003).
                  20 BVVE22-CEVNTO-MSGEM       PIC  X(004).
                  20 BVVE22-CFLUXO-MSGEM       PIC  X(002).
               15 BVVE22-DOMINIO               PIC  X(005).
               15 BVVE22-DT-MOVTO-OPER         PIC  X(010).
               15 BVVE22-CONTR-IF              PIC  X(020).
               15 BVVE22-CMOVTO-RESER          PIC  9(005).
               15 BVVE22-ISPB-ORIGE            PIC  X(008).
               15 BVVE22-ISPB-DSTNO            PIC  X(008).
      *                                                                *
             10 BVVE22-BLOCO-CTRL-REPETICAO.
               15 BVVE22-FIM-BVVEB025          PIC  X(001).
               15 BVVE22-CONT-LINHA-CPO-AVAN   PIC  X(001).
               15 BVVE22-TOT-LINHAS-CONT-AVAN  PIC  9(003).
               15 BVVE22-TOT-LINHAS-EXIB-AVAN  PIC  9(003).
               15 BVVE22-CONT-LINHA-CPO-VOLTA  PIC  X(001).
               15 BVVE22-TOT-LINHAS-CONT-VOLTA PIC  9(003).
               15 BVVE22-TOT-LINHAS-EXIB-VOLTA PIC  9(003).
               15 BVVE22-NORD-CPO-MSGEM-AVAN   PIC  9(003).
               15 BVVE22-NORD-CPO-MSGEM-VOLTA  PIC  9(003).
      *                                                                *
             10 BVVE22-PERSISTENCIA.
               15 BVVE22-AREA                  PIC  X(1000).
      *                                                                *
             10 BVVE22-EXTRA.
               15 BVVE22-NU-OPER               PIC  X(023).
               15 BVVE22-PROC-ERRO             PIC  X(001).
      *                                                                *
             10 BVVE22-TABELA-B015.
               15 BVVE22-SYSMSG-TEX            PIC  X(20000).
               15 BVVE22-SYSMSG-LEN            PIC  9(005).
      *                                                                *
             10 FILLER                         PIC  X(385).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      *****************************************************************
