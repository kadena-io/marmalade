import React, { useState } from "react";
import _ from "lodash";
import ReactMde, {Command, getDefaultCommandMap, getDefaultToolbarCommands} from "react-mde";
import ReactMarkdown from "react-markdown";
import gfm from 'remark-gfm';
import "react-mde/lib/styles/css/react-mde-all.css";

import {
  Chip,
  Container, makeStyles,
} from '@material-ui/core';

const useStyles = makeStyles((theme) => ({
  root: {
      '& p img': {width: "100%"},
  }
}));

const cmdTag = {
  name: "add tag",
  icon: () => <Chip size="small" variant="outline" color="default" label="Tag" style={{marginTop: "-5px"}}/>,
  execute: opts => {
    opts.textApi.replaceSelection("[](tag)");
  }
};

export const MDEditor = (props) => {
  const {value, onChange} = props;
  const [selectedTab, setSelectedTab] = useState("write");
  const classes = useStyles();

  const saveImage = async function (data) {
    // Promise that waits for "time" milliseconds
    alert("No attachments!");
    // returns true meaning that the save was successful
    return "No Attachments";
  };
  const newCmds = getDefaultCommandMap();
  newCmds["cmdTag"] = cmdTag;
  const newCmdBar = getDefaultToolbarCommands();
  newCmdBar.push(["cmdTag"])
  return (
      <ReactMde
        value={value}
        onChange={onChange}
        commands={newCmds}
        toolbarCommands={newCmdBar}
        selectedTab={selectedTab}
        onTabChange={setSelectedTab}
        generateMarkdownPreview={(markdown) => {
          console.log(getTagsFromMdBody(markdown));
          const chips = <ChipsFromTags tags={getTagsFromMd(markdown)}/>
          const strippedOfTags = _.clone(markdown).replaceAll(tagRegex,'');
          const res = Promise.resolve(
          <div>
            {chips}
            <ReactMarkdown remarkPlugins={[gfm]} children={strippedOfTags} className={classes.root}/>
          </div>
            );
          
          console.debug(`react-mde-preview=${res}`, res);
          return res
        }}
        childProps={{
          writeButton: {
            tabIndex: -1
          }
        }}
        paste={{
          saveImage: saveImage
        }}
      />
  );
}

export const RenderMD = ({mdText}) => {
  const classes = useStyles();
  const chips = <ChipsFromTags tags={getTagsFromMd(mdText)}/>
  const strippedOfTags = _.clone(mdText).replaceAll(tagRegex,'');
  return  <Container>
            {chips}
            <ReactMarkdown remarkPlugins={[gfm]} children={strippedOfTags} className={classes.root}/>
          </Container>
}

const tagRegex = /\[([^\[]+)\](\(tag\))/gim;
export const getTagsFromMd = (md) => {
    var m;
    var res = [];
    do {
        m = tagRegex.exec(md);
        if (m) {
            res.push(m[1]);
        }
    } while (m);
  return _.uniq(res.map(v=>v.toLocaleLowerCase()));
};

export const ChipsFromTags = ({tags}) => tags.map(tag=>{
  console.log(tag);
  switch (tag) {
    case "bug hunt":
      return <Chip size="small" color="primary" label="Bug Hunt" style={{backgroundColor:"red"}}/>;
    case "dao":
      return <Chip size="small" color="primary" label="DAO"/>;
    case "frontend":
      return <Chip variant="outlined" size="small" color="primary" label="Frontend"/>;
    case "cold coffee":
      return <Chip size="small" color="default" label="Cold Coffee"/>;
    default:
      return null;

  }
});
  

const regexMatchTags = /\[(.*[^\[]+)\](\(tag\))/gim;

const getTagsFromMdBody = (body) => body.match(regexMatchTags).map(v=>v[1]);
const removeTagsFromMdBody = body => _.clone(body).replace(regexMatchTags,'');
