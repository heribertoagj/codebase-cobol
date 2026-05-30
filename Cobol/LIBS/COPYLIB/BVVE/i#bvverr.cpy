      *==============================================================*
      *   BOOK -  I#BVVERR                                           *
      *           AGENDA/RECEBIVEIS REDECARD                         *
      ****************************************************************
      *   TAMANHO        0240  BYTES                                 *
      ****************************************************************
      *    0 -   REGISTRO HEADER    DO ARQUIVO
      *    1 -   REGISTRO HEADER    DO LOTE
      *    3 -   REGISTRO DETALHE   DO LOTE
      *    5 -   REGISTRO TRAILER   DO LOTE
      *    9 -   REGISTRO TRAILER   DO ARQUIVO
      ***********************************************************
      *----------------------------------------------------------
      *        HEADER DO ARQUIVO = 0
      *----------------------------------------------------------
       01  AG-AGENDA-REDECARD.
           03  AG-HEADER-ARQUIVO.
               05  AGHA-CONTROLE.
                   10  AGHA-CD-BANCO               PIC 9(003).
                   10  AGHA-LOTE-SERVICO           PIC 9(004).
                   10  AGHA-TP-REGISTRO            PIC 9(001).
               05  AGHA-CNAB-01                    PIC X(009).
               05  AGHA-EMPRESA.
                   10  AGHA-INSCRICAO.
                       15  AGHA-TP-INCRICAO        PIC 9(001).
                       15  AGHA-NRO-INCRICAO       PIC 9(014).
                   10  AGHA-CONVENIO-BANCO         PIC X(020).
                   10  AGHA-CONTA-CORRENTE.
                       15  AGHA-AGENCIA.
                           20  AGHA-CD-AGENCIA     PIC X(005).
                           20  AGHA-DV-AGENCIA     PIC X(001).
                       15  AGHA-CONTA.
                           20  AGHA-CD-CONTA       PIC 9(012).
                           20  AGHA-DV-CONTA       PIC X(001).
                       15  AGHA-DV-AGENCIA-CONTA   PIC X(001).
                   10  AGHA-NOME-EMPRESA           PIC X(030).
               05  AGHA-NOME-BANCO                 PIC X(030).
               05  AGHA-CNAB-02                    PIC X(010).
               05  AGHA-ARQUIVO.
                   10  AGHA-CD-REMESSA             PIC 9(001).
                   10  AGHA-DT-GERACAO             PIC 9(008).
                   10  AGHA-HR-GERACAO             PIC 9(006).
                   10  AGHA-SEQUENCIA              PIC 9(006).
                   10  AGHA-VERSAO-LAYOUT          PIC 9(003).
                   10  AGHA-DENSIDADE              PIC 9(005).
               05  AGHA-RESERVADO-BANCO            PIC X(020).
               05  AGHA-RESERVADO-EMPRESA          PIC X(020).
               05  AGHA-CNAB-03                    PIC X(011).
               05  AGHA-COBRANCA-SEM-PAPEL.
                   10  AGHA-ID-VANS                PIC X(003).
                   10  AGHA-CRTL-VANS              PIC 9(003).
                   10  AGHA-SERVICO                PIC X(002).
                   10  AGHA-OCORRENCIAS            PIC X(010).
      *----------------------------------------------------------
      *        HEADER DO LOTE = 1
      *----------------------------------------------------------
           03  AG-HEADER-LOTE     REDEFINES AG-HEADER-ARQUIVO.
               05  AGHL-CONTROLE.
                   10  AGHL-CD-BANCO               PIC 9(003).
                   10  AGHL-LOTE-SERVICO           PIC 9(004).
                   10  AGHL-TP-REGISTRO            PIC 9(001).
               05  AGHL-SERVICO.
                   10  AGHL-TP-OPERACAO            PIC X(001).
                   10  AGHL-TP-SERVICO             PIC 9(002).
                   10  AGHL-FORMA-LANCTO           PIC 9(002).
                   10  AGHL-VERSAO-LOTE            PIC 9(003).
               05  AGHL-CNAB-01                    PIC X(001).
               05  AGHL-EMPRESA.
                   10  AGHL-INSCRICAO.
                       15  AGHL-TP-INCRICAO        PIC 9(001).
                       15  AGHL-NRO-INCRICAO       PIC 9(014).
                   10  AGHL-CONVENIO-BANCO         PIC X(020).
                   10  AGHL-CONTA-CORRENTE.
                       15  AGHL-AGENCIA.
                           20  AGHL-CD-AGENCIA     PIC X(005).
                           20  AGHL-DV-AGENCIA     PIC X(001).
                       15  AGHL-CONTA.
                           20  AGHL-CD-CONTA       PIC 9(012).
                           20  AGHL-DV-CONTA       PIC X(001).
                       15  AGHL-DV-AGENCIA-CONTA   PIC X(001).
                   10  AGHL-NOME-EMPRESA           PIC X(030).
               05  AGHL-MENSAGEM                   PIC X(040).
               05  AGHL-ENDERECO-EMPRESA.
                   10  AGHL-ENDERECO               PIC X(030).
                   10  AGHL-NUMERO                 PIC 9(005).
                   10  AGHL-ENDERECO-COMPL         PIC X(015).
                   10  AGHL-CIDADE                 PIC X(020).
                   10  AGHL-CEP-001                PIC 9(005).
                   10  AGHL-CEP-002                PIC X(003).
                   10  AGHL-ESTADO                 PIC X(002).
               05  AGHL-CNAB-02                    PIC X(008).
               05  AGHL-OCORRENCIAS                PIC X(010).
      *----------------------------------------------------------
      *        DETALHE DO LOTE = 3
      *----------------------------------------------------------
           03  AG-DETALHE-LOTE    REDEFINES AG-HEADER-ARQUIVO.
               05  AGDL-CONTROLE.
                   10  AGDL-CD-BANCO               PIC 9(003).
                   10  AGDL-LOTE-SERVICO           PIC 9(004).
                   10  AGDL-TP-REGISTRO            PIC 9(001).
               05  AGDL-SERVICO.
                   10  AGDL-NRO-REGISTRO           PIC 9(005).
                   10  AGDL-CD-SEGMENTO            PIC X(001).
                   10  AGDL-MOVIMENTO.
                       15  AGDL-TP-MOVTO           PIC 9(001).
                       15  AGDL-CD-MOVTO           PIC 9(002).
               05  AGDL-FAVORECIDO.
                   10  AGDL-COMPENSACAO            PIC 9(003).
                   10  AGDL-CD-BANCO-FAVOR         PIC 9(003).
                   10  AGDL-CONTA-CORRENTE.
                       15  AGDL-AGENCIA.
                           20  AGDL-CD-AGENCIA     PIC X(005).
                           20  AGDL-DV-AGENCIA     PIC X(001).
                       15  AGDL-CONTA.
                           20  AGDL-CD-CONTA       PIC 9(012).
                           20  AGDL-DV-CONTA       PIC X(001).
                       15  AGDL-DV-AGENCIA-CONTA   PIC X(001).
                   10  AGDL-NOME-EMPRESA           PIC X(030).
               05  AGDL-CREDITO.
                   10  AGDL-SEU-NRO                PIC X(020).
                   10  AGDL-DT-PAGTO               PIC 9(008).
                   10  AGDL-MOEDA.
                       15  AGDL-TP-MOEDA           PIC X(003).
                       15  AGDL-QTD-MOEDA          PIC 9(010)V9(5).
                   10  AGDL-VLR-PAGTO              PIC 9(013)V9(2).
                   10  AGDL-NOSSO-NRO              PIC X(020).
                   10  AGDL-DT-REAL                PIC 9(008).
                   10  AGDL-VLR-REAL               PIC 9(013)V9(2).
               05  AGDL-INFORMACAO                 PIC X(040).
               05  AGDL-CD-FINALIDADE-DOC          PIC X(002).
               05  AGDL-CNAB-01                    PIC X(010).
               05  AGDL-AVISO                      PIC 9(001).
               05  AGDL-OCORRENCIAS                PIC X(010).
      *----------------------------------------------------------
      *        TRAILER DO LOTE = 5
      *----------------------------------------------------------
           03  AG-TRAILER-LOTE    REDEFINES AG-HEADER-ARQUIVO.
               05  AGTL-CONTROLE.
                   10  AGTL-CD-BANCO               PIC 9(003).
                   10  AGTL-LOTE-SERVICO           PIC 9(004).
                   10  AGTL-TP-REGISTRO            PIC 9(001).
               05  AGTL-CNAB-01                    PIC X(009).
               05  AGTL-TOTAIS.
                   10  AGTL-QTD-REGISTROS          PIC 9(006).
                   10  AGTL-SOMA-VALORES           PIC 9(016)V9(2).
                   10  AGTL-SOMA-MOEDAS            PIC 9(013)V9(5).
               05  AGTL-NRO-AVISO-DEB              PIC 9(006).
               05  AGTL-CNAB-02                    PIC X(165).
               05  AGTL-OCORRENCIAS                PIC X(010).
      *----------------------------------------------------------
      *        TRAILER DO ARQUIVO = 9
      *----------------------------------------------------------
           03  AG-TRAILER-ARQUIVO REDEFINES AG-HEADER-ARQUIVO.
               05  AGTA-CONTROLE.
                   10  AGTA-CD-BANCO               PIC 9(003).
                   10  AGTA-LOTE-SERVICO           PIC 9(004).
                   10  AGTA-TP-REGISTRO            PIC 9(001).
               05  AGTA-CNAB-01                    PIC X(009).
               05  AGTA-TOTAIS.
                   10  AGTA-QTD-LOTES              PIC 9(006).
                   10  AGTA-QTD-REGISTROS          PIC 9(006).
                   10  AGTA-QTD-CONTAS-CONCIL      PIC 9(006).
               05  AGTA-CNAB-02                    PIC X(205).
